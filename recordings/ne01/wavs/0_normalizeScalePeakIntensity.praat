Create Strings as file list... soundFiles C:\Users\lb1181\Desktop\pa_4\recordings\ne01\*.wav
select Strings soundFiles
numberOfFiles = Get number of strings

for i to numberOfFiles
	select Strings soundFiles
	soundName$ = Get string... i
	Read from file...  C:\Users\lb1181\Desktop\pa_4\recordings\ne01\'soundName$'
	Scale peak... 0.99
	Write to binary file...  C:\Users\lb1181\Desktop\pa_4\recordings\ne01\'soundName$'
	Remove
endfor