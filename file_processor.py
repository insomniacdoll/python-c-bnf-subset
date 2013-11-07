import re

def comments_deleter(src_code=''):
	regex = r'\/\*(\s|.)*?\*\/'
	result, number = re.subn(regex, "", src_code)
	return result

def file_reader(file_path=''):
	result = ''
	if(file_path != ''):
		try:
			fd = open(file_path, 'r')
			while True:
				lines = fd.readlines(100000)
				if not lines:
					break
				for line in lines:
					result = result + line
			fd.close()
			result = comments_deleter(result)
			return result
		except:
			print('source file %s read error' % file_path)
	else:
		print('file path is null.')

'''
def dump_func_defs_from_str(src_code=''):
	regex = r'(?:(extern|static)(?:\s+))?(?:((?:[a-zA-Z_]\w*)(?:(?:\s(?:\s*\*\s*)*)|(?:(?:\s*\*\s*)*\s)))([a-zA-Z_]\w*)\s*)(\([^\)]*\)?)\s*(?:(\{(?:[^\{\}]*)(?:(?:(?'brace'\{)[^\{\}]*)+(?:(?'-brace'\})[^\{\}]*)+)*(?(brace)(?!))\})|;)'
	p = re.compile(regex)
	m = p.match(src_code)
	print(m.group())
'''
#test lines...
#dump_func_defs_from_str('main(int a){}')
#print(file_reader('C:\Dev\Workspaces\C\linux-3.5.4\linux-3.5.4\ipc\msg.c'))
#test_str = '/** sss ***/ sdsds /** sss ***/'
#print(comments_deleter(test_str).strip())
