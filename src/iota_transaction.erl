% Copyright (c) 2017 Solarbit.cc <steve@solarbit.cc>
% See LICENCE

-module(iota_transaction).
-include("iota.hrl").
-compile(export_all).

-record(iota_transaction, {
	hash, signature, address, value, tag,
	timestamp, current_index, last_index,
	bundle, trunk, branch, nonce
}).


encode(TX = #iota_transaction{}) ->
	ok.


decode(TX) -> % when byte_size(TX) =:= 2673 ->
	Hash = iota_hash:curl(TX),
	<<Signature:2187/binary, Address:81/binary, Value:11/binary, Zeros:16/binary,
		Tag:27/binary, Timestamp:9/binary, CurrentIndex:9/binary, LastIndex:9/binary,
		Bundle:81/binary, Trunk:81/binary, Branch:81/binary, Nonce:81/binary, More/binary>> = TX,
	<<"9999999999999999">> = Zeros,
	?TTY({unparsed, More}),
	#iota_transaction {
		hash = Hash,
		signature = Signature,
		address = Address,
		value = trinary:to_integer(Value),
		tag = Tag,
		timestamp = trinary:to_integer(Timestamp),
		current_index = trinary:to_integer(CurrentIndex),
		last_index = trinary:to_integer(LastIndex),
		bundle = Bundle,
		trunk = Trunk,
		branch = Branch,
		nonce = Nonce
	}.



test() ->
	TX = <<"VOGXPLLACZ99SOANI9ZUZCYCPPCFWFSJQFQROCEYANRYSUEYS9EWJNAKVOYSCYPY9OHPTAMAFZDJUPXEXTVNOFNQAPG9EDJAEWQJAAIUQMTPSZWQ9DPNDDEVKWO9RGJZOEMWTQB9DDPGZDNKYDIPCR9GDAVQ9ZBMZDLJUVBZRYNSMBZKLHNMXFPPLHJUPDRATGJDXNYPOTYBLIJSFKTBJTQUSYRHYGXIUZFZBFZKOQVVYMFSFV9EFPDAJOWUVGVHAMMLX9YDU9ROQASFDPQRBQRLXQNG9SCNSLQLQRDSUGBSNRO9ZBVLRHVPC9TXQY9KMONXESKSSICQEIPBIAYNTGNJOMBYSZDTFMPXJQPWYXWCF9DQ9NBDIQQJQFEFNWJR9WHHXKICVWQAFETEJ99PZHTGXDJCVOMJRDYUTZWXXQTPBLLFWDDPDVSFFITLWDTHMKWLFRELKO9OARSWDSPZAUJJQRSPCF9ETZGVTRKHABSQTGUNDCVRWHMBIKMWOKTMXD9IFJIEARLUNFZSOUEISGXKAWUSJNKGV9UWUQZZQIXIQTTKSTLMEVSLYLOZDLCSLSBECFUDUYUQJYTUXF9OSLMLRKOLQTPCXAKRPEHJUGJDICSIPPYZDEFLPSENYO9YZZF9E9SCPPYXDCMWRUYBLCJNR9HOIXLGKKNCTTI9CMPRXDZR9AGUXJEVQPOEWDTKNHKHJUKIYSKMTLNQBTFVIIQONAKY999DOSBJA9FSDUDLHACVIKIWHQALEGQULRXYFFLXPAGWTLQ9UQPWOACSASXETSNCGEXBQADLXYQHWSOBQIGZOBYQLGAOVCKOGTOBZWCBIULXTLQIEBPZOQZCFPOHKREDPSJMDHDRNJZORACJLHNVBGMP9PCCMAHWCODIVTWGDPGJTOTIELPQIGVI9QZDTGLWBBYRSMRTWLQQTWS9DESQCZUDBJRLLGNSNOWKHJQCMTSZETOCDOMXYIJDPTYROJRNYAGQCPAGVPPNUFDVB9UTPVUUL9GGDPAFCBJEPHXUEBNMDCABRJDBTSWYEADQUFJEEKZQZS9FDKOQUDQIDKIMBAPGMRRXAUXALLZOPSCXOWDNLWQTVZOVKC9QNQTCQMTTSXPSEFKEYRFKVKVQDRMDROJSUNOE9SESSEL9GDQVCIJVSMDKAUBCOTILWDJKZTCNHCYDNMYVQPDLMMLJSEFHUXJKXYSUTGTSRTFUCXDRNRQHEZWPUUAXBQCWFVBRHFKUTQBQKHUGABTKOZPUONJMYTSTXEENKPEGYJOSELYLVYIJONELBK9RWYMXBEWIVQYMVIBUECAQP9CXIDJSWOVCUKMBJDN9GFMPMUWAWBMGHTIDZVMSGSFNRRXEMWLOMMDCGGONUMHTBXUTONDGOWJXVPQCJTQFBDYIDQACCPXBMHEO99IJSYSDT9CLJSQFBSQOX9P9NDHZXMBWQWRSTQJXUASCMDTKKAYDQW9YW9XUBMDG9CEZCU9BAYAUHRQKUXIRVGWQACRAHEWREIOAFCTSHTERIXAXGXIANCHEYIQXRVSSIVUXX9K9GPXHT9ITYO9RUGNWGTEXIEIEFMMHFZQXRCOIP9BIEMKDWHLLWETGV9UNMLERNIZDWRULWRR9GTEYXWWKQSLMKCPJAJDXKJDDDDLSOSOCKMNNHARWBXLGKSTA9FYKMPGXLEXFSMHLBCXOJPVNPODFAJRPAXGL9KBVYLOANIAO9G99JEIUMUVGMUYPTXYGLPUPGNNLLOM9IIBBBJ9WCHOAQKEJAFPXRFQKZGXAMZLCOXB9LWEBWOJ9JVULOLIUJQPJQIOILIYWXWHWGXACFHZLIEEPGPJLG9KKEWXOHGXLEEMTUZZKPDFMTAPD9OAOFACMWBMISLQCNGLWFBS9KPFACIGXXCPRWYMOGMBFFP9LGWRCIZBVCZDCRGUPZPKMCWENMRUVWA9MBKDELHHH9ELZDKCNVWBSGAPMAFFKE9RMHTGSQQXINFJHNIKTHZGXSROTUWD9JFMCQZDBQGGD9ZSHLKWMEJYWKLIBNCQOUSIRQCBTOKMDTNDIBXEBBTQCWVEVGJFPFXPGRVADNSWLSKVYKACSCKFPQBJUHWLKQZST9VIGUHLFRHMVSJCYTMYLF9MCNSLDDTDCM9JKBEYIZTASJBPSGUQYCEQ9LLHPGUJKHTUDKBZNYANFNIXNQSG">>,
	?TTY({size, byte_size(TX)}),
	decode(TX).