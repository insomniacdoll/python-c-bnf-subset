# -*- coding: utf-8 -*-

from datetime import datetime
from optparse import OptionParser
import os, logging

from lib.database import *
from lib.CLibrary import *
from lib.CParser import *


class KernelCrawler():

    def __init__(self, host='127.0.0.1:6000', database='kernel', user='root', password='123456', srcRoot='linux'):

        # create a database connection instance
        self.db = Connection(host, database, user, password)

        # kernel src root
        self.srcRoot = srcRoot

        self.file_relation_list = []

    def resolve_src(self):
        
        logging.debug('Root path : %s'%self.srcRoot)
        for root, dirs, files in os.walk(self.srcRoot, True):
            for name in files:
                if(name[-2:] != '.c' and name[-2:] != '.h'):
                    continue
                try:
                    #print(os.path.join(root, name))
                    p = CParser(os.path.join(root, name))
                    p.processAll()
                    p.printAll()
                    #print p.callFuncDict
                    if(len(p.callFuncDict) > 0):
                        for each_caller in p.callFuncDict.keys():
                            #print each_caller 
                            # get function status from function info table
                            select_non_exportfunc_sql = 'select Id from functioninfo where FunctionName = \'%s\' and FunctionType = 0'%each_caller
                            logging.debug(select_non_exportfunc_sql)
                            select_non_exportfunc_result = self.db.query(select_non_exportfunc_sql) 
                            logging.debug(select_non_exportfunc_result)

                            # get file id and module id from filename table
                            relative_filepath = os.path.join(root, name)[os.path.join(root, name).find("linux-3.5.4")+11:]
                            logging.debug(relative_filepath)
                            select_caller_module_sql = 'select Id, ModuleId from filename where FileName = \'%s\''%relative_filepath
                            logging.debug(select_caller_module_sql)
                            select_caller_module_result = self.db.query(select_caller_module_sql)
                            logging.debug(select_caller_module_result)
                            if(len(select_caller_module_result) > 0):
                                caller_module_id = select_caller_module_result[0]['ModuleId']
                                caller_file_id = select_caller_module_result[0]['Id']
                            else:
                                caller_module_id = -1
                                caller_file_id = -1
                            
                            # if current function def is not a export function, then save it
                            if(len(select_non_exportfunc_result) == 0):
                                insert_non_exportfunc_sql = 'insert into functioninfo(FunctionName, FileNameId, FunctionType) values (\'%s\', %s, 0)'%(each_caller, caller_file_id)
                                self.db.execute(insert_non_exportfunc_sql)
                                logging.debug(insert_non_exportfunc_sql)

                            # for each called function
                            for each_called in p.callFuncDict[each_caller]:
                                logging.debug(p.callFuncDict[each_caller])
                                # get called function status
                                select_export_sql = 'select Id, FileNameId from functioninfo where FunctionName = \'%s\' and FunctionType = 1'%each_called[0]
                                select_export_result = self.db.query(select_export_sql)
                                logging.debug(select_export_sql)
                                logging.debug(select_export_result)
                                if(len(select_export_result) > 0):
                                    called_id = select_export_result[0]['Id'] 
                                    called_file_id = select_export_result[0]['FileNameId']
                                
                                # if called function is export
                                if(len(select_export_result) > 0):

                                    # get caller's id
                                    select_caller_id_sql = 'select Id from functioninfo where FunctionName = \'%s\''%(each_caller)
                                    select_caller_id_result = self.db.query(select_caller_id_sql)
                                    logging.debug(select_caller_id_sql)
                                    logging.debug(select_caller_id_result)
                                    caller_id = select_caller_id_result[0]['Id']

                                    # then get called function's moduleid
                                    select_called_module_sql = 'select ModuleId from filename where Id = %s'%select_export_result[0]['FileNameId']
                                    select_called_module_result = self.db.query(select_called_module_sql)
                                    logging.debug(select_called_module_sql)
                                    logging.debug(select_called_module_result)
                                    called_module_id = select_called_module_result[0]['ModuleId']
                                    
                                    # save the call relation into db
                                    insert_functioncall_sql = 'insert into functioncall (CallerId, CallerModuleId, CalleeId, CalleeModuleId, Conditions) values (%s, %s, %s, %s, \'%s\')'%(caller_id, caller_module_id, called_id, called_module_id, each_called[1])
                                    self.db.execute(insert_functioncall_sql)
                                    logging.debug(insert_functioncall_sql)
                                    
                                    # check if the file relation has been saved
                                    if((caller_file_id, called_file_id) not in self.file_relation_list):
                                        insert_filerelation_sql = 'insert into filerelation (fromId, toId) values (%s, %s)'%(caller_file_id, called_file_id) 
                                        self.db.execute(insert_filerelation_sql)
                                        self.file_relation_list.append((caller_file_id, called_file_id))
                                        logging.debug(insert_filerelation_sql)

                    if(len(p.defs['macros']) > 0):
                        for each_macro in p.defs['macros'].keys():
                            #print each_macro
                            
                            # get file id from filename table
                            relative_filepath = os.path.join(root, name)[os.path.join(root, name).find("linux-3.5.4")+11:]
                            logging.debug(relative_filepath)
                            select_fileid_sql = 'select Id from filename where FileName = \'%s\''%relative_filepath
                            logging.debug(select_fileid_sql)
                            select_fileid_result = self.db.query(select_fileid_sql)
                            logging.debug(select_fileid_result)
                            if(len(select_fileid_result) > 0):
                                file_id = select_fileid_result[0]['Id']
                            else:
                                file_id = -1
                            
                            macrotype = 0
                            macroexpr = p.defs['macros'][each_macro]
                           
                            macroexpr = macroexpr.replace('%', '%%')
                            insert_macro_sql = 'insert into macroinfo(MacroName, FileNameId, MacroType, MacroExpr) values (\'%s\', %s, %s, \'%s\')' % (each_macro, file_id, macrotype, macroexpr)
                            logging.debug(insert_macro_sql)
                            ret = self.db.execute(insert_macro_sql)
                            logging.debug('insert macro sql execute ret is: %s' % ret)

                    if(len(p.defs['fnmacros']) > 0):
                        for each_macro in p.defs['fnmacros'].keys():
                            #print each_macro
                            
                            # get file id from filename table
                            relative_filepath = os.path.join(root, name)[os.path.join(root, name).find("linux-3.5.4")+11:]
                            logging.debug(relative_filepath)
                            select_fileid_sql = 'select Id from filename where FileName = \'%s\''%relative_filepath
                            logging.debug(select_fileid_sql)
                            select_fileid_result = self.db.query(select_fileid_sql)
                            logging.debug(select_fileid_result)
                            if(len(select_fileid_result) > 0):
                                file_id = select_fileid_result[0]['Id']
                            else:
                                file_id = -1
                            
                            macrotype = 1
                            macroexpr = p.defs['fnmacros'][each_macro][0]
                            
                            macroexpr = macroexpr.replace('%', '%%')
                            insert_macro_sql = 'insert into macroinfo(MacroName, FileNameId, MacroType, MacroExpr) values (\'%s\', %s, %s, \'%s\')' % (each_macro, file_id, macrotype, macroexpr)
                            logging.debug(insert_macro_sql)
                            ret = self.db.execute(insert_macro_sql)
                            logging.debug('insert macro sql execute ret is: %s' % ret)


                    if(len(p.defs['structs']) > 0):
                        for each_struct in p.defs['structs'].keys():
                            #print each_struct
                            
                            # get file id from filename table
                            relative_filepath = os.path.join(root, name)[os.path.join(root, name).find("linux-3.5.4")+11:]
                            logging.debug(relative_filepath)
                            select_fileid_sql = 'select Id from filename where FileName = \'%s\''%relative_filepath
                            logging.debug(select_fileid_sql)
                            select_fileid_result = self.db.query(select_fileid_sql)
                            logging.debug(select_fileid_result)
                            if(len(select_fileid_result) > 0):
                                file_id = select_fileid_result[0]['Id']
                            else:
                                file_id = -1
                            
                            structtype = 1
                            
                            insert_struct_sql = 'insert into typeinfo(TypeName, TypeKind, FileNameId) values (\'%s\', %s, %s)' % (each_struct, structtype, file_id)
                            logging.debug(insert_struct_sql)
                            ret = self.db.execute(insert_struct_sql)
                            logging.debug('insert struct sql execute ret is: %s' % ret)


                    if(len(p.defs['values']) > 0):
                        for each_value in p.defs['values'].keys():
                            #print each_value
                            
                            # get file id from filename table
                            relative_filepath = os.path.join(root, name)[os.path.join(root, name).find("linux-3.5.4")+11:]
                            logging.debug(relative_filepath)
                            select_fileid_sql = 'select Id from filename where FileName = \'%s\''%relative_filepath
                            logging.debug(select_fileid_sql)
                            select_fileid_result = self.db.query(select_fileid_sql)
                            logging.debug(select_fileid_result)
                            if(len(select_fileid_result) > 0):
                                file_id = select_fileid_result[0]['Id']
                            else:
                                file_id = -1
                            
                            valuetype = 2
                            
                            insert_value_sql = 'insert into typeinfo(TypeName, TypeKind, FileNameId) values (\'%s\', %s, %s)' % (each_value, valuetype, file_id)
                            logging.debug(insert_value_sql)
                            ret = self.db.execute(insert_value_sql)
                            logging.debug('insert value sql execute ret is: %s' % ret)
                
                except Exception, e:
                    logging.debug('skip file : %s'%os.path.join(root, name))

if __name__ == '__main__':
    logging.basicConfig(filename= 'log/' + str(datetime.now().strftime('%Y-%m-%d')) + '.log', level=logging.DEBUG)

    usage = "usage: python %prog [options] arg1 arg2"
    parser = OptionParser(usage=usage)
    parser.add_option("-o", "--host",\
        action="store", dest="host", default='127.0.0.1',\
        help="mysql host name [default : 127.0.0.1]")
    parser.add_option("-u", "--username",\
        action="store", dest="username", default='root',\
        help="mysql username [default : root]")
    parser.add_option("-p", "--password",\
        action="store", dest="password", default='123456',\
        help="mysql password [default : 123456]")
    parser.add_option("-d", "--database",\
        action="store", dest="database", default='kernel',\
        help="mysql database name [default : kernel]")
    parser.add_option("-r", "--port",\
        action="store", dest="port", default='3306',\
        help="mysql port [default : 3306]")
    parser.add_option("-s", "--kernelsrc",\
        action="store", dest="kernelsrc", default='linux',\
        help="linux kernel source path [default : linux]")
    parser.add_option("-a", "--arch",\
        action="store", dest="arch", default='x86',\
        help="linux kernel architecture [default : x86]")
    (options, args) = parser.parse_args()
    
    root_path = options.kernelsrc + '/'
    sub_folders = ['drivers', 'lib', 'mm', 'security', 'usr', 'block', 'crypto', 'firmware', 'init', 'sound', 'virt', 'fs', 'ipc', 'kernel']
    if(options.arch == 'x86'):
        sub_folders.append('arch/x86')
    else:       
        sub_folders.append('arch/arm')
    for each_sub_folder in sub_folders:
        each_full_path = root_path + each_sub_folder
        each_host = options.host + ':' + options.port
        k = KernelCrawler(host=each_host, user=options.username, password=options.password, database=options.database, srcRoot=each_full_path)
        k.resolve_src()


