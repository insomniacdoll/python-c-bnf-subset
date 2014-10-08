# -*- coding: utf-8 -*-
"""
CParser.py - C parsing library 
"""

import sys, re, os, time

__all__ = ['winDefs', 'CParser']


def winDefs(verbose=False):
    headerFiles = ['WinNt.h', 'WinDef.h', 'WinBase.h', 'BaseTsd.h', 'WTypes.h', 'WinUser.h']
    d = os.path.dirname(__file__)
    p = CParser(
        [os.path.join(d, 'headers', h) for h in headerFiles],
        types={'__int64': ('long long')},
        macros={'_WIN32': '', '_MSC_VER': '800', 'CONST': 'const', 'NO_STRICT': None},
        processAll=False
    )
    p.processAll(cache=os.path.join(d, 'headers', 'WinDefs.cache'), noCacheWarning=True, verbose=verbose)
    return p


class CParser():
        
    cacheVersion = 22    ## increment every time cache structure or parsing changes to invalidate old cache files.
    
    def __init__(self, files=None, replace=None, copyFrom=None, processAll=True, cache=None, verbose=False, **args):        
        
        self.defs = {}  ## holds all definitions
        self.fileDefs = {}  ## holds definitions grouped by the file they came from
        
        self.initOpts = args.copy()
        self.initOpts['files'] = []
        self.initOpts['replace'] = {}
        
        self.dataList = ['types', 'variables', 'fnmacros', 'macros', 'structs', 'unions', 'enums', 'functions', 'values']
            
        self.verbose = False
            
        # placeholders for definitions that change during parsing
        #if hasPyParsing:
            #self.macroExpr = Forward()
            #self.fnMacroExpr = Forward()
            #self.definedType = Forward()
            #self.definedStruct = Forward()
            #self.definedEnum = Forward()
        
        self.fileOrder = []
        self.files = {}
        self.packList = {}  ## list describing struct packing rules as defined by #pragma pack
        if files is not None:
            if type(files) is str:
                files = [files]
            for f in files:
                self.loadFile(f, replace)
                    
        ## initialize empty definition lists
        for k in self.dataList:
            self.defs[k] = {}
            #for f in files:
                #self.fileDefs[f][k] = {}
                
        self.compiledTypes = {}  ## holds translations from typedefs/structs/unions to fundamental types
                
        self.currentFile = None

        self.callFuncDict = {}  ## holds all function call relations in current file
        
        # Import extra arguments if specified
        for t in args:
            for k in args[t].keys():
                self.addDef(t, k, args[t][k])
        
        # Import from other CParsers if specified
        if copyFrom is not None:
            if type(copyFrom) not in [list, tuple]:
                copyFrom = [copyFrom]
            for p in copyFrom:
                self.importDict(p.fileDefs)
                
        if processAll:
            self.processAll(cache=cache, verbose=verbose)
    
    def processAll(self, cache=None, returnUnparsed=False, printAfterPreprocess=False, noCacheWarning=True, verbose=False):
        
        self.verbose = verbose
        if cache is not None and self.loadCache(cache, checkValidity=True):
            if verbose:
                print "Loaded cached definitions; will skip parsing."
            return  ## cached values loaded successfully, nothing left to do here
        #else:
            #print "No cache.", cache
            
        
        results = []
        if noCacheWarning or verbose:
            pass
            #print "Parsing C header files (no valid cache found). This could take several minutes..."
        for f in self.fileOrder:
            #fn = os.path.basename(f)
            if self.files[f] is None:
                ## This means the file could not be loaded and there was no cache.
                raise Exception('Could not find header file "%s" or a suitable cache file.' % f)
            if verbose:
                print "Removing comments from file '%s'..." % f
            self.removeComments(f)
            if verbose:
                print "Preprocessing file '%s'..." % f
            self.preprocess(f)
            if printAfterPreprocess:
                print "===== PREPROCSSED %s =======" % f
                print self.files[f]
            if verbose:
                print "Parsing definitions in file '%s'..." % f
            results.append(self.parseDefs(f, returnUnparsed))
        
        if cache is not None:
            if verbose:
                print "Writing cache file '%s'" % cache
            self.writeCache(cache)
            
        return results
        
            
    def loadCache(self, cacheFile, checkValidity=False):
        
        ## make sure cache file exists 
        if type(cacheFile) is not str:
            raise Exception("cache file option must be a string.")
        if not os.path.isfile(cacheFile):
            d = os.path.dirname(__file__)  ## If file doesn't exist, search for it in this module's path
            cacheFile = os.path.join(d, "headers", cacheFile)
            if not os.path.isfile(cacheFile):
                if self.verbose:
                    print "Can't find requested cache file."
                return False
        
        ## make sure cache is newer than all input files
        if checkValidity:
            mtime = os.stat(cacheFile).st_mtime
            for f in self.fileOrder:
                ## if file does not exist, then it does not count against the validity of the cache.
                if os.path.isfile(f) and os.stat(f).st_mtime > mtime:
                    if self.verbose:
                        print "Cache file is out of date."
                    return False
        
        try:
            ## read cache file
            import pickle
            cache = pickle.load(open(cacheFile, 'rb'))
            
            ## make sure __init__ options match
            if checkValidity:
                if cache['opts'] != self.initOpts:
                    if self.verbose:
                        print "Cache file is not valid--created using different initialization options."
                        print cache['opts']
                        print self.initOpts
                    return False
                elif self.verbose:
                    print "Cache init opts are OK:"
                    print cache['opts']
                if cache['version'] < self.cacheVersion:
                    if self.verbose:
                        print "Cache file is not valid--cache format has changed."
                    return False
                
            ## import all parse results
            self.importDict(cache['fileDefs'])
            return True
        except:
            print "Warning--cache read failed:"
            sys.excepthook(*sys.exc_info())
            return False

    def importDict(self, data):
        for f in data.keys():
            self.currentFile = f
            for k in self.dataList:
                for n in data[f][k]:
                    self.addDef(k, n, data[f][k][n])

    def writeCache(self, cacheFile):
        cache = {}
        cache['opts'] = self.initOpts
        cache['fileDefs'] = self.fileDefs
        cache['version'] = self.cacheVersion
        #for k in self.dataList:
            #cache[k] = getattr(self, k)
        import pickle
        pickle.dump(cache, open(cacheFile, 'wb'))

    def loadFile(self, file, replace=None):
        if not os.path.isfile(file):
            ## Not a fatal error since we might be able to function properly if there is a cache file..
            #raise Exception("File %s not found" % file)
            print "Warning: C header '%s' is missing; this may cause trouble." % file
            self.files[file] = None
            return False
            
        fd = open(file, 'rU')  ## U causes all newline types to be converted to \n
        self.files[file] = fd.read()
        fd.close()
        
        if replace is not None:
            for s in replace:
                self.files[file] = re.sub(s, replace[s], self.files[file])
        self.fileOrder.append(file)
        bn = os.path.basename(file)
        self.initOpts['replace'][bn] = replace
        self.initOpts['files'].append(bn) # only interested in the file names; the directory may change between systems.
        return True
    
    #### Beginning of processing functions
    
    def assertPyparsing(self):
        global hasPyParsing
        if not hasPyParsing:
            raise Exception("CParser class requires 'pyparsing' library for actual parsing work. Without this library, CParser can only be used with previously cached parse results.")
    

    def removeComments(self, file):
        self.assertPyparsing()
        text = self.files[file]
        cplusplusLineComment = Literal("//") + restOfLine
        # match quoted strings first to prevent matching comments inside quotes
        self.files[file] = (quotedString | cStyleComment.suppress() | cplusplusLineComment.suppress()).transformString(text)
        
    
    def preprocess(self, file):
        self.assertPyparsing()
        self.buildParser()  ## we need this so that evalExpr works properly
        self.currentFile = file
        packStack = [(None,None)]  ## stack for #pragma pack push/pop
        self.packList[file] = [(0,None)]
        packing = None  ## current packing value 
        
        text = self.files[file]
        
        ## First join together lines split by \\n
        text = Literal('\\\n').suppress().transformString(text)
        
        #self.ppDirective = Combine("#" + Word(alphas).leaveWhitespace()) + restOfLine
        
        # define the structure of a macro definition
        name = Word(alphas+'_', alphanums+'_')('name')
        self.ppDefine = name.setWhitespaceChars(' \t')("macro") + Optional(lparen + delimitedList(name) + rparen).setWhitespaceChars(' \t')('args') + SkipTo(LineEnd())('value')
        self.ppDefine.setParseAction(self.processMacroDefn)
        
        #self.updateMacroDefns()
        #self.updateFnMacroDefns()

        # define pattern for scanning through the input string
        #self.macroExpander = (self.macroExpr | self.fnMacroExpr)
        
        ## Comb through lines, process all directives
        lines = text.split('\n')
        
        result = []
        #macroExpander = (quotedString | self.macroExpander)
        directive = re.compile(r'\s*#([a-zA-Z]+)(.*)$')
        ifTrue = [True]
        ifHit = []
        for i in range(len(lines)):
            line = lines[i]
            newLine = ''
            m = directive.match(line)
            if m is None:  # regular code line
                if ifTrue[-1]:  # only include if we are inside the correct section of an IF block
                    #line = macroExpander.transformString(line)  # expand all known macros
                    newLine = self.expandMacros(line)
            else:  # macro line
                d = m.groups()[0]
                rest = m.groups()[1]
                
                #print "PREPROCESS:", d, rest
                if d == 'ifdef':
                    d = 'if'
                    rest = 'defined '+rest
                elif d == 'ifndef':
                    d = 'if'
                    rest = '!defined '+rest
                    
                ## Evaluate 'defined' operator before expanding macros
                if d in ['if', 'elif']:
                    def pa(t):
                        return ['0', '1'][t['name'] in self.defs['macros'] or t['name'] in self.defs['fnmacros']]
                    rest = (
                        Keyword('defined') + 
                        (name | lparen + name + rparen)
                    ).setParseAction(pa).transformString(rest)
                elif d in ['define', 'undef']:    
                    macroName, rest = re.match(r'\s*([a-zA-Z_][a-zA-Z0-9_]*)(.*)$', rest).groups()
                
                ## Expand macros if needed
                if rest is not None and (all(ifTrue) or d in ['if', 'elif']):
                    rest = self.expandMacros(rest)
                    
                if d == 'elif':
                    if ifHit[-1] or not all(ifTrue[:-1]):
                        ev = False
                    else:
                        ev = self.evalPreprocessorExpr(rest)
                    if self.verbose:
                        print "  "*(len(ifTrue)-2) + line, rest, ev
                    ifTrue[-1] = ev
                    ifHit[-1] = ifHit[-1] or ev
                elif d == 'else':
                    if self.verbose:
                        print "  "*(len(ifTrue)-2) + line, not ifHit[-1]
                    ifTrue[-1] = (not ifHit[-1]) and all(ifTrue[:-1])
                    ifHit[-1] = True
                elif d == 'endif':
                    ifTrue.pop()
                    ifHit.pop()
                    if self.verbose:
                        print "  "*(len(ifTrue)-1) + line
                elif d == 'if':
                    if all(ifTrue):
                        ev = self.evalPreprocessorExpr(rest)
                    else:
                        ev = False
                    if self.verbose:
                        print "  "*(len(ifTrue)-1) + line, rest, ev
                    ifTrue.append(ev)
                    ifHit.append(ev)
                elif d == 'define':
                    if not ifTrue[-1]:
                        continue
                    if self.verbose:
                        print "  "*(len(ifTrue)) + "define:", macroName, rest
                    try:
                        self.ppDefine.parseString(macroName+ ' ' + rest) ## macro is registered here
                    except:
                        print "Error processing macro definition:", macroName, rest
                        print "      ", sys.exc_info()[1]
                elif d == 'undef':
                    if not ifTrue[-1]:
                        continue
                    try:
                        self.remDef('macros', macroName.strip())
                        #self.macroListString = '|'.join(self.defs['macros'].keys() + self.defs['fnmacros'].keys())
                        #self.updateMacroDefns()
                    except:
                        if sys.exc_info()[0] is not KeyError:
                            sys.excepthook(*sys.exc_info())
                            print "Error removing macro definition '%s'" % macroName.strip()
                elif d == 'pragma':  ## Check for changes in structure packing
                    if not ifTrue[-1]:
                        continue
                    m = re.match(r'\s+pack\s*\(([^\)]+)\)', rest)
                    if m is None:
                        continue
                    opts = [s.strip() for s in m.groups()[0].split(',')]
                    
                    pushpop = id = val = None
                    for o in opts:
                        if o in ['push', 'pop']:
                            pushpop = o
                        elif o.isdigit():
                            val = int(o)
                        else:
                            id = o
                            
                    if val is not None:
                        packing = val
                        
                    if pushpop == 'push':
                        packStack.append((packing, id))
                    elif opts[0] == 'pop':
                        if id is None:
                            packStack.pop()
                        else:
                            ind = None
                            for i in range(len(packStack)):
                                if packStack[i][1] == id:
                                    ind = i
                                    break
                            if ind is not None:
                                packStack = packStack[:ind]
                        if val is None:
                            packing = packStack[-1][0]
                    else:
                        packing = int(opts[0])
                    
                    if self.verbose:
                        print ">> Packing changed to %s at line %d" % (str(packing), i)
                    self.packList[file].append((i, packing))
                else:
                    pass  ## Ignore any other directives
                    
            result.append(newLine)      
        self.files[file] = '\n'.join(result)
        
    def evalPreprocessorExpr(self, expr):
        ## make a few alterations so the expression can be eval'd
        macroDiffs = (
            Literal('!').setParseAction(lambda: ' not ') | 
            Literal('&&').setParseAction(lambda: ' and ') | 
            Literal('||').setParseAction(lambda: ' or ') | 
            Word(alphas+'_',alphanums+'_').setParseAction(lambda: '0'))
        expr2 = macroDiffs.transformString(expr)
            
        try:
            ev = bool(eval(expr2))
        except:
            if self.verbose:
                print "Error evaluating preprocessor expression: %s [%s]" % (expr, expr2)
                print "      ", sys.exc_info()[1]
            ev = False
        return ev
            
        
        
    #def updateMacroDefns(self):
        ##self.macroExpr << MatchFirst( [Keyword(m)('macro') for m in self.defs['macros']] )
        ##self.macroExpr.setParseAction(self.processMacroRef)
        
        ## regex is faster than pyparsing.
        ## Matches quoted strings and macros
        
        ##names = self.defs['macros'].keys() + self.defs['fnmacros'].keys()
        #if len(self.macroListString) == 0:
            #self.macroRegex = None
        #else:
            #self.macroRegex = re.compile(
                #r'("(\\"|[^"])*")|(\b(%s)\b)'   %   self.macroListString
            #)

    #def updateFnMacroDefns(self):
        #self.fnMacroExpr << MatchFirst( [(Keyword(m)('macro') + lparen + Group(delimitedList(expression))('args') + rparen) for m in self.defs['fnmacros']] )
        #self.fnMacroExpr.setParseAction(self.processFnMacroRef)        
        
    
    def processMacroDefn(self, t):
        if self.verbose:
            print "MACRO:", t
        #macroVal = self.macroExpander.transformString(t.value).strip()
        #macroVal = Literal('\\\n').suppress().transformString(macroVal) ## remove escaped newlines
        macroVal = t.value.strip()
        if macroVal in self.defs['fnmacros']:
            self.addDef('fnmacros', t.macro, self.defs['fnmacros'][macroVal])
            if self.verbose:
                print "  Copy fn macro %s => %s" % (macroVal, t.macro) 
        else:
            if t.args == '':
                val = self.evalExpr(macroVal)
                self.addDef('macros', t.macro, macroVal)
                self.addDef('values', t.macro, val)
                if self.verbose:
                    print "  Add macro:", t.macro, "("+str(val)+")", self.defs['macros'][t.macro]
            else:
                self.addDef('fnmacros', t.macro,  self.compileFnMacro(macroVal, [x for x in t.args]))
                if self.verbose:
                    print "  Add fn macro:", t.macro, t.args, self.defs['fnmacros'][t.macro]
        
        #if self.macroListString == '':
            #self.macroListString = t.macro
        #else:
            #self.macroListString += '|' + t.macro
        #self.updateMacroDefns()
        #self.macroExpr << MatchFirst( map(Keyword,self.defs['macros'].keys()) )
        return "#define " + t.macro + " " + macroVal
    
    
    def compileFnMacro(self, text, args):
        ## find all instances of each arg in text
        argRegex = re.compile(r'("(\\"|[^"])*")|(\b(%s)\b)'   %   ('|'.join(args)))
        start = 0
        parts = []
        argOrder = []
        N = 3
        for m in argRegex.finditer(text):
            arg = m.groups()[N]
            #print m, arg
            if arg is not None:
                parts.append(text[start:m.start(N)] + '%s')
                start = m.end(N)
                argOrder.append(args.index(arg))
        parts.append(text[start:])
        return (''.join(parts), argOrder)
        
    
    def expandMacros(self, line):
        reg = re.compile(r'("(\\"|[^"])*")|(\b(\w+)\b)')
        parts = []
        start = 0
        N = 3   ## the group number to check for macro names
        macros = self.defs['macros']
        fnmacros = self.defs['fnmacros']
        for m in reg.finditer(line):
            name = m.groups()[N]
            if name in macros:
                parts.append(line[start:m.start(N)])
                start = m.end(N)
                parts.append(macros[name])
            elif name in fnmacros:
                try:  ## If function macro expansion fails, just ignore it.
                    exp, end = self.expandFnMacro(name, line[m.end(N):])
                    parts.append(line[start:m.start(N)])
                    start = end + m.end(N)
                    parts.append(exp)
                except:
                    if sys.exc_info()[1][0] != 0:
                        print "Function macro expansion failed:", name, line[m.end(N):]
                        raise
        parts.append(line[start:])
        return ''.join(parts)
                


    #def expandMacros(self, line):
        #if self.macroRegex is None:
            #return line
        #parts = []
        #start = 0
        #N = 3   ## the group number to check for macro names
        #for m in self.macroRegex.finditer(line):
            #name = m.groups()[N]
            #if name is not None:
                #if name in self.defs['macros']:
                    #parts.append(line[start:m.start(N)])
                    #start = m.end(N)
                    #parts.append(self.defs['macros'][name])
                #elif name in self.defs['fnmacros']:
                    #try:  ## If function macro expansion fails, just ignore it.
                        #exp, end = self.expandFnMacro(name, line[m.end(N):])
                        #parts.append(line[start:m.start(N)])
                        #start = end + m.end(N)
                        #parts.append(exp)
                    #except:
                        #if sys.exc_info()[1][0] != 0:
                            #print "Function macro expansion failed:", name, line[m.end(N):]
                            #raise
                        
                #else:
                    #raise Exception("Macro '%s' not found (internal error)" % name)
        #parts.append(line[start:])
        #return ''.join(parts)

    def expandFnMacro(self, name, text):
        #print "expandMacro:", name, text
        defn = self.defs['fnmacros'][name]
        ## defn looks like ('%s + %s / %s', (0, 0, 1))
        
        argList = stringStart + lparen + Group(delimitedList(expression))('args') + rparen
        res = [x for x in argList.scanString(text, 1)]
        if len(res) == 0:
            raise Exception(0, "Function macro '%s' not followed by (...)" % name)
        args, start, end = res[0]
        #print "  ", res 
        #print "  ", args
        #print "  ", defn
        newStr = defn[0] % tuple([args[0][i] for i in defn[1]])
        #print "  ", newStr
        return (newStr, end)
        
        
    # parse action to replace macro references with their respective definition
    #def processMacroRef(self, t):
        #return self.defs['macros'][t.macro]
            
    #def processFnMacroRef(self, t):
        #m = self.defs['fnmacros'][t.macro]
        ##print "=====>>"
        ##print "Process FN MACRO:", t
        ##print "  macro defn:", t.macro, m
        ##print "  macro call:", t.args
        ### m looks like ('a + b', ('a', 'b'))
        #newStr = m[0][:]
        ##print "  starting str:", newStr
        #try:
            #for i in range(len(m[1])):
                ##print "  step", i
                #arg = m[1][i]
                ##print "    arg:", arg, '=>', t.args[i]
                
                #newStr = Keyword(arg).copy().setParseAction(lambda: t.args[i]).transformString(newStr)
                ##print "    new str:", newStr
        #except:
            ##sys.excepthook(*sys.exc_info())
            #raise
        ##print "<<====="
        #return newStr        
        
    def parseDefs(self, file, returnUnparsed=False):
        self.assertPyparsing()
        self.currentFile = file
        #self.definedType << kwl(self.defs['types'].keys())
    
        parser = self.buildParser()
        if returnUnparsed:
            text = parser.suppress().transformString(self.files[file])
            return re.sub(r'\n\s*\n', '\n', text)
        else:
            return [x[0] for x in parser.scanString(self.files[file])]

    def buildParser(self):
        
        if hasattr(self, 'parser'):
            return self.parser
            
            
        self.assertPyparsing()
        
        
        self.structType = Forward()
        self.enumType = Forward()
        self.typeSpec = (typeQualifier + (
            fundType | 
            Optional(kwl(sizeModifiers + signModifiers)) + ident | 
            self.structType | 
            self.enumType
        ) + typeQualifier + msModifier).setParseAction(recombine)
        #self.argList = Forward()
        
        self.declarator = Forward()
        self.abstractDeclarator = Forward()
        
        self.abstractDeclarator << Group(
            typeQualifier + Group(ZeroOrMore('*'))('ptrs') + typeQualifier +
            ((Optional('&')('ref')) | (lparen + self.abstractDeclarator + rparen)('center')) + 
            Optional(lparen + Optional(delimitedList(Group(
                self.typeSpec('type') + 
                self.abstractDeclarator('decl') + 
                Optional(Literal('=').suppress() + expression, default=None)('val')
            )), default=None) + rparen)('args') + 
            Group(ZeroOrMore(lbrack + Optional(expression, default='-1') + rbrack))('arrays')
        )
        
        self.declarator << Group(
            typeQualifier + callConv + Group(ZeroOrMore('*'))('ptrs') + typeQualifier +
            ((Optional('&')('ref') + ident('name')) | (lparen + self.declarator + rparen)('center')) + 
            Optional(lparen + Optional(delimitedList(Group(
                self.typeSpec('type') + 
                (self.declarator | self.abstractDeclarator)('decl') + 
                Optional(Literal('=').suppress() + expression, default=None)('val')
            )), default=None) + rparen)('args') + 
            Group(ZeroOrMore(lbrack + Optional(expression, default='-1') + rbrack))('arrays')
        )
        self.declaratorList = Group(delimitedList(self.declarator))

        ## typedef
        self.typeDecl = Keyword('typedef') + self.typeSpec('type') + self.declaratorList('declList') + semi
        self.typeDecl.setParseAction(self.processTypedef)

        ## variable declaration
        self.variableDecl = Group(self.typeSpec('type') + Optional(self.declaratorList('declList')) + Optional(Literal('=').suppress() + (expression('value') | (lbrace + Group(delimitedList(expression))('arrayValues') + rbrace)))) + semi
        
        self.variableDecl.setParseAction(self.processVariable)
        
        ## function definition
        #self.paramDecl = Group(self.typeSpec + (self.declarator | self.abstractDeclarator)) + Optional(Literal('=').suppress() + expression('value'))
        #self.typelessFunctionDecl = self.declarator('decl') + nestedExpr('{', '}').suppress()
        self.functionBody = Forward()
        self.functionBody << nestedExpr('{', '}')
        self.functionDecl = self.typeSpec('type') + self.declarator('decl') + self.functionBody('body')
        self.typelessFunctionDecl = self.declarator('decl') + self.functionBody('body')
        self.typelessFunctionDecl.setParseAction(self.processFunction)
        self.functionDecl.setParseAction(self.processFunction)
        
        
        ## Struct definition
        self.structDecl = Forward()
        structKW = (Keyword('struct') | Keyword('union'))
        #self.structType << structKW('structType') + ((Optional(ident)('name') + lbrace + Group(ZeroOrMore( Group(self.structDecl | self.variableDecl.copy().setParseAction(lambda: None)) ))('members') + rbrace) | ident('name'))
        self.structMember = (
            Group(self.variableDecl.copy().setParseAction(lambda: None)) |
            (self.typeSpec + self.declarator + nestedExpr('{', '}')).suppress() |
            (self.declarator + nestedExpr('{', '}')).suppress()
        )
        self.declList = lbrace + Group(OneOrMore(self.structMember))('members') + rbrace
        self.structType << (Keyword('struct') | Keyword('union'))('structType') + ((Optional(ident)('name') + self.declList) | ident('name'))
        
        self.structType.setParseAction(self.processStruct)
        #self.updateStructDefn()
        
        self.structDecl = self.structType + semi

        ## enum definition
        enumVarDecl = Group(ident('name')  + Optional(Literal('=').suppress() + (integer('value') | ident('valueName'))))
        
        self.enumType << Keyword('enum') + (Optional(ident)('name') + lbrace + Group(delimitedList(enumVarDecl))('members') + Optional(",") + rbrace | ident('name'))
        self.enumType.setParseAction(self.processEnum)
        
        self.enumDecl = self.enumType + semi

        
        #self.parser = (self.typeDecl | self.variableDecl | self.structDecl | self.enumDecl | self.functionDecl)
        self.parser = (self.typeDecl | self.variableDecl | self.functionDecl)
        return self.parser
    
    def processDeclarator(self, decl):
        toks = []
        name = None
        #print "DECL:", decl
        if 'callConv' in decl and len(decl['callConv']) > 0:
            toks.append(decl['callConv'])
        if 'ptrs' in decl and len(decl['ptrs']) > 0:
            toks.append('*' * len(decl['ptrs']))
        if 'arrays' in decl and len(decl['arrays']) > 0:
            #arrays  = []
            #for x in decl['arrays']:
                #n = self.evalExpr(x)
                #if n == -1:           ## If an array was given as '[]', interpret it as '*' instead.
                    #toks.append('*')
                #else:
                    #arrays.append(n)
            #if len(arrays) > 0:
                #toks.append(arrays)
            toks.append([self.evalExpr(x) for x in decl['arrays']])
        if 'args' in decl and len(decl['args']) > 0:
            #print "  process args"
            if decl['args'][0] is None:
                toks.append(())
            else:
                toks.append(tuple([self.processType(a['type'], a['decl']) + (a['val'][0],) for a in decl['args']]))
        if 'ref' in decl:
            toks.append('&')
        if 'center' in decl:
            (n, t) = self.processDeclarator(decl['center'][0])
            if n is not None:
                name = n
            toks.extend(t)
        if 'name' in decl:
            name = decl['name']
        return (name, toks)
    
    def processType(self, typ, decl):
        #print "PROCESS TYPE/DECL:", typ, decl
        (name, decl) = self.processDeclarator(decl)
        return (name, [typ] + decl)            

    def processEnum(self, s, l, t):
        try:
            if self.verbose:
                print "ENUM:", t
            if t.name == '':
                n = 0
                while True:
                    name = 'anonEnum%d' % n
                    if name not in self.defs['enums']:
                        break
                    n += 1
            else:
                name = t.name[0]
                
            if self.verbose:
                print "  name:", name
                
            if name not in self.defs['enums']:
                i = 0
                enum = {}
                for v in t.members:
                    if v.value != '':
                        i = eval(v.value)
                    if v.valueName != '':
                        i = enum[v.valueName]
                    enum[v.name] = i
                    self.addDef('values', v.name, i)
                    i += 1
                if self.verbose:
                        print "  members:", enum
                self.addDef('enums', name, enum)
                self.addDef('types', 'enum '+name, ('enum', name))
            return ('enum ' + name)
        except:
            if self.verbose:
                print "Error processing enum:", t
            sys.excepthook(*sys.exc_info())

    def _call_function_parse(self, functionname='unknow', stmt=None, condition=""):
        i = 0
        stmt_pass_word = ['if', 'for', 'while', 'case', 'switch', 'else']
        stmt_len = len(stmt)
        local_condition = ""
        while i < stmt_len:
        #for each_raw_elem in stmt:
            #print i
            #time.sleep(1)
            each_raw_elem = stmt[i]
            #print each_raw_elem
            if(type(each_raw_elem) == type('str') and each_raw_elem not in stmt_pass_word):
                each_elem = each_raw_elem.strip('()')
                if('(' in each_elem):
                    left_index = each_elem.index('(')
                    if(left_index > 0):
                        called_func_name = each_elem[:left_index]
                        #print 'condition is : ' + condition
                        #print 'called function is : ' + called_func_name
                        if (not self.callFuncDict.has_key(functionname)):
                            self.callFuncDict[functionname] = []
                        self.callFuncDict[functionname].append((called_func_name, condition))
                        i = i + 1
                else:
                    i = i + 1

                if(condition.find('&&&') != -1): 
                    if(i-3>=0 and stmt[i-3] == 'if'):
                        local_condition = condition[0:condition.rfind('&&&')]
                    if(i-5>=0 and stmt[i-5] == 'if'):
                        local_condition = condition[0:condition.rfind('&&&')]
                elif(condition != "" and condition.find('&&&') == -1):  
                    if(i-3>=0 and stmt[i-3] == 'if'):
                        local_condition = ""   
                    if(i-5>=0 and stmt[i-5] == 'if'):
                        local_condition = "" 
            
            elif(type(each_raw_elem) == type('str') and each_raw_elem == 'if'):
                local_condition = stmt[i+1]
                #j = i
                #print local_condition
                #print stmt[i+1]
                if(stmt[i+1][0] == '(' and stmt[i+1][len(stmt[i+1])-1] != ')'):
                    i = i + 1
                    while(stmt[i][len(stmt[i])-1] != ')'):
                        local_condition = local_condition + stmt[i + 1]
                        i = i + 1
                else:
                    i = i + 1
                if condition != "":
                    local_condition = condition + ' &&& ' + local_condition
                #condition = local_condition
            #print type(each_raw_elem)
            elif type(each_raw_elem) != type('str'):
                self._call_function_parse(functionname,stmt=each_raw_elem, condition=local_condition)
                if(local_condition != "" and local_condition.find('&&&') != -1):
                    local_condition = local_condition[0:local_condition.rfind('&&&')]
                elif(local_condition != "" and local_condition.find('&&&') == -1):
                    local_condition = ''
                else:
                    local_condition = ''
                i = i + 1
            else:
                i = i + 1

    def processFunction(self, s, l, t):
        if self.verbose:
            print "FUNCTION", t, t.keys()
        
        try: 
            (name, decl) = self.processType(t.type, t.decl[0])
            #print '======function name====='
            #print name
            #print '======function body====='
            body = t.body[0]
            #print body

            #self._call_function_parse(stmt=body)
 
            if len(decl) == 0 or type(decl[-1]) != tuple:
                #print t
                #print decl[:-1]
                #raise Exception("Incorrect declarator type for function definition.")
                print "Incorrect declarator type for function definition."
            if self.verbose:
                print "  name:", name
                print "  sig:", decl
                print "  body:", body
            self.addDef('functions', name, (decl[:-1], decl[-1], body))
            
        except:
            if self.verbose:
                print "Error processing function:", t
            sys.excepthook(*sys.exc_info())


    def packingAt(self, line):
        packing = None
        for p in self.packList[self.currentFile]:
            if p[0] <= line: 
                packing = p[1]
            else:
                break
        return packing

    def processStruct(self, s, l, t):
        try:
            strTyp = t.structType  # struct or union
            
            ## check for extra packing rules
            packing = self.packingAt(lineno(l, s))
            
            if self.verbose:
                print strTyp.upper(), t.name, t
            if t.name == '':
                n = 0
                while True:
                    sname = 'anon_%s%d' % (strTyp, n)
                    if sname not in self.defs[strTyp+'s']:
                        break
                    n += 1
            else:
                if type(t.name) is str:
                    sname = t.name
                else:
                    sname = t.name[0]
            if self.verbose:
                print "  NAME:", sname
            if len(t.members) > 0 or sname not in self.defs[strTyp+'s'] or self.defs[strTyp+'s'][sname] == {}:
                if self.verbose:
                    print "  NEW " + strTyp.upper()
                struct = []
                for m in t.members:
                    typ = m[0].type
                    val = self.evalExpr(m)
                    if self.verbose:
                        print "    member:", m, m[0].keys(), m[0].declList
                    if len(m[0].declList) == 0:  ## anonymous member
                        struct.append((None, [typ], None))
                    for d in m[0].declList:
                        (name, decl) = self.processType(typ, d)
                        struct.append((name, decl, val))
                        if self.verbose:
                            print "      ", name, decl, val
                self.addDef(strTyp+'s', sname, {'pack': packing, 'members': struct})
                self.addDef('types', strTyp+' '+sname, (strTyp, sname))
                #self.updateStructDefn()
            return strTyp+' '+sname
        except:
            #print t
            sys.excepthook(*sys.exc_info())

    def processVariable(self, s, l, t):
        if self.verbose:
            print "VARIABLE:", t
        try:
            val = self.evalExpr(t[0])
            for d in t[0].declList:
                (name, typ) = self.processType(t[0].type, d)
                if type(typ[-1]) is tuple:  ## this is a function prototype
                    if self.verbose:
                        print "  Add function prototype:", name, typ, val
                    self.addDef('functions', name, (typ[:-1], typ[-1]))
                else:
                    if self.verbose:
                        print "  Add variable:", name, typ, val
                    self.addDef('variables', name, (val, typ))
                    self.addDef('values', name, val)
        except:
            #print t, t[0].name, t.value
            sys.excepthook(*sys.exc_info())

    def processTypedef(self, s, l, t):
        if self.verbose:
            print "TYPE:", t
        typ = t.type
        #print t, t.type
        for d in t.declList:
            (name, decl) = self.processType(typ, d)
            if self.verbose:
                print "  ", name, decl
            self.addDef('types', name, decl)
            #self.definedType << MatchFirst( map(Keyword,self.defs['types'].keys()) )
        
    def evalExpr(self, toks):
        try:
            if isinstance(toks, basestring):
                #print "  as string"
                val = self.eval(toks, None, self.defs['values'])
            elif toks.arrayValues != '':
                #print "  as list:", toks.arrayValues
                val = [self.eval(x, None, self.defs['values']) for x in toks.arrayValues]
            elif toks.value != '':
                #print "  as value"
                val = self.eval(toks.value, None, self.defs['values'])
            else:
                #print "  as None"
                val = None
            return val
        except:
            if self.verbose:
                print "    failed eval:", toks
                print "                ", sys.exc_info()[1]
            return None
            
    def eval(self, expr, *args):
        expr = expr.strip()
        cast = (lparen + self.typeSpec + self.abstractDeclarator + rparen).suppress()
        expr = (quotedString | number | cast).transformString(expr)
        if expr == '':
            return None
        return eval(expr, *args)
        
    def printAll(self, file=None):
        from pprint import pprint
        for k in self.dataList:
            #print "============== %s ==================" % k
            
            # start resolving functions...
            if (k == 'functions'):
                for functionname in self.defs[k].keys():
                    #print '==============='
                    #print functionname
                    #print '----------'
                    self._call_function_parse(functionname,stmt=self.defs[k][functionname][2])
                    #print '==============='
                #self._call_function_parse()

            if file is None:
                #pprint(self.defs[k])
                pass
            else:
                #pprint(self.fileDefs[file][k])
                pass
                
    def addDef(self, typ, name, val):
        self.defs[typ][name] = val
        if self.currentFile is None:
            baseName = None
        else:
            baseName = os.path.basename(self.currentFile)
        if baseName not in self.fileDefs:
            self.fileDefs[baseName] = {}
            for k in self.dataList:
                self.fileDefs[baseName][k] = {}
        self.fileDefs[baseName][typ][name] = val

    def remDef(self, typ, name):
        if self.currentFile is None:
            baseName = None
        else:
            baseName = os.path.basename(self.currentFile)
        del self.defs[typ][name]
        del self.fileDefs[baseName][typ][name]
        

    def isFundType(self, typ):
        if typ[0][:7] == 'struct ' or typ[0][:6] == 'union ' or typ[0][:5] == 'enum ':
            return True
            
        names = baseTypes + sizeModifiers + signModifiers
        for w in typ[0].split():
            if w not in names:
                return False
        return True

    def evalType(self, typ):
        used = []
        while True:
            if self.isFundType(typ):
                ## remove 'signed' before returning evaluated type
                typ[0] = re.sub(r'\bsigned\b', '', typ[0]).strip()
                
                
                return typ
            parent = typ[0]
            if parent in used:
                raise Exception('Recursive loop while evaluating types. (typedefs are %s)' % (' -> '.join(used+[parent])))
            used.append(parent)
            if not parent in self.defs['types']:
                raise Exception('Unknown type "%s" (typedefs are %s)' % (parent, ' -> '.join(used)))
            pt = self.defs['types'][parent]
            typ = pt + typ[1:]

    def find(self, name):
        res = []
        for f in self.fileDefs:
            fd = self.fileDefs[f]
            for t in fd:
                typ = fd[t]
                for k in typ:
                    if isinstance(name, basestring):
                        if k == name:
                            res.append((f, t))
                    else:
                        if re.match(name, k):
                            res.append((f, t, k))
        return res

    
    
    def findText(self, text):
        res = []
        for f in self.files:
            l = self.files[f].split('\n')
            for i in range(len(l)):
                if text in l[i]:
                    res.append((f, i, l[i]))
        return res
        
        
hasPyParsing = False
try: 
    from pyparsing import *
    ParserElement.enablePackrat()
    hasPyParsing = True
except:
    pass  ## no need to do anything yet as we might not be using any parsing functions..


## Define some common language elements if pyparsing is available.
if hasPyParsing:
    ## Some basic definitions
    expression = Forward()
    pexpr = '(' + expression + ')'
    numTypes = ['int', 'float', 'double', '__int64']
    baseTypes = ['char', 'bool', 'void'] + numTypes
    sizeModifiers = ['short', 'long']
    signModifiers = ['signed', 'unsigned']
    qualifiers = ['const', 'static', 'volatile', 'inline', 'restrict', 'near', 'far']
    msModifiers = ['__based', '__declspec', '__fastcall', '__restrict', '__sptr', '__uptr', '__w64', '__unaligned', '__nullterminated']
    keywords = ['struct', 'enum', 'union', '__stdcall', '__cdecl'] + qualifiers + baseTypes + sizeModifiers + signModifiers

    def kwl(strs):
        """Generate a match-first list of keywords given a list of strings."""
        #return MatchFirst(map(Keyword,strs))
        return Regex(r'\b(%s)\b' % '|'.join(strs))

    keyword = kwl(keywords)
    wordchars = alphanums+'_$'
    ident = (WordStart(wordchars) + ~keyword + Word(alphas+"_",alphanums+"_$") + WordEnd(wordchars)).setParseAction(lambda t: t[0])
    #integer = Combine(Optional("-") + (Word( nums ) | Combine("0x" + Word(hexnums)))) 
    semi   = Literal(";").ignore(quotedString).suppress()
    lbrace = Literal("{").ignore(quotedString).suppress()
    rbrace = Literal("}").ignore(quotedString).suppress()
    lbrack = Literal("[").ignore(quotedString).suppress()
    rbrack = Literal("]").ignore(quotedString).suppress()
    lparen = Literal("(").ignore(quotedString).suppress()
    rparen = Literal(")").ignore(quotedString).suppress()
    hexint = Regex('-?0x[%s]+[UL]*'%hexnums).setParseAction(lambda t: t[0].rstrip('UL'))
    decint = Regex(r'-?\d+[UL]*').setParseAction(lambda t: t[0].rstrip('UL'))
    integer = (hexint | decint)
    floating = Regex(r'-?((\d+(\.\d*)?)|(\.\d+))([eE]-?\d+)?')
    number = (hexint | floating | decint)
    bitfieldspec = ":" + integer
    biOperator = oneOf("+ - / * | & || && ! ~ ^ % == != > < >= <= -> . :: << >> = ? :")
    uniRightOperator = oneOf("++ --")
    uniLeftOperator = oneOf("++ -- - + * sizeof new")
    name = (WordStart(wordchars) + Word(alphas+"_",alphanums+"_$") + WordEnd(wordchars))
    #number = Word(hexnums + ".-+xUL").setParseAction(lambda t: t[0].rstrip('UL'))
    #stars = Optional(Word('*&'), default='')('ptrs')  ## may need to separate & from * later?
    callConv = Optional(Keyword('__cdecl')|Keyword('__stdcall'))('callConv')
    
    ## Removes '__name' from all type specs.. may cause trouble.
    underscore2Ident = (WordStart(wordchars) + ~keyword + '__' + Word(alphanums,alphanums+"_$") + WordEnd(wordchars)).setParseAction(lambda t: t[0])
    typeQualifier = ZeroOrMore((underscore2Ident + Optional(nestedExpr())) | kwl(qualifiers)).suppress()
    
    msModifier = ZeroOrMore(kwl(msModifiers) + Optional(nestedExpr())).suppress()
    pointerOperator = (
        '*' + typeQualifier |
        '&' + typeQualifier |
        '::' + ident + typeQualifier
    )


    ## language elements
    fundType = OneOrMore(kwl(signModifiers + sizeModifiers + baseTypes)).setParseAction(lambda t: ' '.join(t))

    ## Is there a better way to process expressions with cast operators??
    castAtom = (
        ZeroOrMore(uniLeftOperator) + Optional('('+ident+')').suppress() + 
        ((
            ident + '(' + Optional(delimitedList(expression)) + ')' | 
            ident + OneOrMore('[' + expression + ']') | 
            ident | number | quotedString
        )  |
        ('(' + expression + ')')) + 
        ZeroOrMore(uniRightOperator)
    )
    uncastAtom = (
        ZeroOrMore(uniLeftOperator) + 
        ((
            ident + '(' + Optional(delimitedList(expression)) + ')' | 
            ident + OneOrMore('[' + expression + ']') | 
            ident | number | quotedString
        )  |
        ('(' + expression + ')')) + 
        ZeroOrMore(uniRightOperator)
    )
    atom = castAtom | uncastAtom

    expression << Group(
        atom + ZeroOrMore(biOperator + atom)
    )
    arrayOp = lbrack + expression + rbrack

    def recombine(tok):
        return " ".join(flatten(tok.asList()))
    expression.setParseAction(recombine)
            
    def flatten(lst):
        res = []
        for i in lst:
            if type(i) in [list, tuple]:
                res.extend(flatten(i))
            else:
                res.append(str(i))
        return res

    def printParseResults(pr, depth=0, name=''):
        start = name + " "*(20-len(name)) + ':'+ '..'*depth    
        if isinstance(pr, ParseResults):
            print start
            for i in pr:
                name = ''
                for k in pr.keys():
                    if pr[k] is i:
                        name = k
                        break
                printParseResults(i, depth+1, name)
        else:
            print start  + str(pr)   
