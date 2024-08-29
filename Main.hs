import Enigma


enigma1 = (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
plugboard = [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')] 
enigma2 = (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25) [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')])
enigma3 = (SteckeredEnigma rotor2 rotor4 rotor3 reflectorB (0,0,25) [('F','X'),('D','M'),('E','A'),('S','T')])
enigma4 = (SteckeredEnigma rotor3 rotor2 rotor1 reflectorB (0,0,25) [('C','R'),('L','N'),('E','P'),('S','I')])


testEnigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (3,9,2)
p1 = "WETTERVORHERSAGEBISKAYA"
x1 = encodeMessage p1 testEnigma


p2 =  "THEMAINPROFESSIONALINSTITUTIONFORCOMPUTING"
x2a = "QKPKFNLRTLQHVIGRIKUOEWSXRTIHWIODZORBRQJFZLMAJXKVXBPHROGJSPVIFNQERCRSZATNJXORIKPYEDBMYCLOCTHAZCSFSTGMRJCVICAJDUUETLYLFGJAFQZTDUTVPPYVROVPCGFYZTKWJEAKZZQXQCQUBMKDHYHAVIXKDLHRFCGTAADGJZLTVTWAKXHZNDFVRBUBTLWGBYXSOTVGMPSKCODKAOUCQJPJPEOIMAEMSSNVINMVVLFCMGVQCQOKAEWLVBSRBKTMXRUJOSUTCWRDHDYPHPHNVLEGVHERDSMTEUUQNRNVALCZGKRKXTNDCLLTQHVBOQYMYQMCZEZYBDKQOXRVFZBEUUYMKIYZNBDMVSFXWPUMDMGTCABLEBAPAKFQCXOPWIVDTMTIKCVIRALQKVSLVQHBFOMWRVKOWWJRMCBBQETMFBQBDCIMXMZPFJYNPVDJJFJMYGKHFDLQCYTFBXGQGRVQFKUHJQYTXQPSAJUHLSVKMJOSKRUHLDADOKQQPVEJGPWLNHYJUBDHCVBMMEPAJLUSECKYLCFMVFQLTPBYOHREQMOCDTWUXOGREDIVWJSXKHWJTKUHZHVEIIKGEEVZIMLKLMJAVYCIRWBQCSVXLTJBEXZYDJNTLWTRPNCMJWSPWQUGBUDLWQRUFYMHMNAASAHRMDREKWHTIOVZMBOTDUZHMWSMJNJIJWHEBBVGJKTHHYYCWUMTGJKROEZZKJZQDJUIOUPVIIYZIROQISYALZDHUPYTHYTLOPLFDKQZMBUOCXNOBUGGGCGQMXKUJKUUPFFGAJJIKCZRENJQLHCKTMUDFQKDZYCVGIULRKGUCOBJIGSESZXSCJYWHXGBDYGCHCIFWQYPVMBBHYNLKSZRFVYIFFNYEETKHLNLBGKWBGRIMFOVEIWHRIYQSVADSDKPYDOIKXQTUYRHPXJZFHYUQADBVLVDGMDCGLOORPZQBTVNBAPIJGSMJRHKFKSRSFSUVZDGYCSQFXDMKWKNACWXQOSQUOCJGICHNGRKIAXKQMYWHDFDEEKZJZQDBQCTAXSZYUHXLHMYJ"
x2b = "QKPKFNLRTLQHVEGRIKUOEWSXRTIHWIODZORBRQJSZLMAJXKVXBPHROGJSPVIFNQERGDDANKEGLZLUVCRHOCESEHBSQXQDZZKVMIAABJERUTULCVSZQKEZLTOMSNLIHXKVFTYPLDUNSCPCGRJGMLVOPEMKFKOWWNUAGWGEGOELYJENXUSQICSJULMANOWVWMRSXKUVCKOOILRDQTPLUMTQBMSGYUMZOMILBJRTXFMYQCVXFBTUKHCSVGSYOUICPPEOCHTPVFDMKMSQHZBEUZVIXLIMASEFVINKNDMXFKRVOPGOGWFFTLGSQPOAPENFIWOCPKTDJIBYXBZOACKOSAWUNFVEGDPAMZQMAAAZAKTHTCBOVBPUUJAYDSGJRETBUBYXLKIOSKROZLKUFIMMAMFBUZKFLGCQKJXAYJZPWUZXBGIWZNPYPELVJUSBMGFMKPMJXWOCFWQDTZTRSOBMRKKQGGQZRHVMUACCBQKBVAHOELDNTXXWFHIQOGPOFAGZUJMROBDMSMPUGMNZVSZXXVJFCNWVAMOWPVGPJGMPZHEXKHRIXIFCJMXZCPTTOYJOVNYSVWCXBBKCNVRAGJQWXYIXDAOEBQUZMTUSOBEGLKCLZEULAIBDACUXLXVYRWVQIGWDRYRSQIUUFDJUBPGGDVZHDBYQBMNWSSWLFGXMGANDUWKAWLEIKMOLNNAXKDPHNULCGFTSYYZNCOKZJRPDRYXHIQWAPLVBGQULAZPUIJSCISWPGMMUKEMYFWCKCWXCJOTRZVPFLYADFZWNMBHLZQPUAHYFGGHCCKJSSBTMPUBFWLEWNWJSDYFCKFAVUHDYYGNYADBPTFQMXJZPQTHVSYSLHSTOBWGKDYEPRFNLZJEGOKJMXDZBXRLGTGHOAIDZVTJZSPQAEHKHRBFDLOCDQOAJEYACHXMXFMFYUODNVPIJZNYVIFHZWHRJVQKGTQANDUYZTGRRFQVQMLNJDJAXSTLVIMEPYPHEZMYYKHXGSCZBCRDHXHMEGFCOTHCDVETICYWEDVPXJGJLAAOQLANLGPEUUSIKQMWXNTYHOPEG"
x2c = "QKPKFNLRTLQHVIGRIKUOEWSXRTIHWIODZORBRQJFZLMAJXKVXBPHROGJSPVIFNQERCDDANKEGLZLUVCRHOCESEHBSQXGPBLUZDVKIWNKNPQTBBSRQVBTGFTOMSNLIHXKVFTYPLDUNSCPCGRDGMLVOPEMKFKOWWNUAGWGEGOELKJENXUSQICSJULMANOWVWMRSXKPVCKOOILRDQTPLUMTQBMSGYUMZGMILBJRTXFMYQCVXFBTUKHCSVGIYOUICPPEOCHTPVFDMKMSQHZBEYZVIXLIMASEFVINKNDMXFKRVOPVOGWFFTLGSQPOAPENFIWOCPKTDDIBYXBZOACKOSAWUNFVEGDPAMZWMAAAZAKTHTCBOVBPUUJAYDSGJMETBUBYXLKIOSKROZLKUFIMMAMKBUZKFLGCQKJXAYJZPWUZXBGIWQNPYPELVJUSBMGFMKPMJXWOCFWCDTZTRSOBMRKKQGGQZRHVMUACCIQKBVAHOELDNTXXWFHIQOGPOFAUZUJMROBDMSMPUGMNZVSZXXVJFZNWVAMOWPVGPJGMPZHEXKHRIXINCJMXZCPTTOYJOVNYSVWCXBBKCYVRAGJQWXYIXDAOEBQUZMTUSOBEGLKCLZEULAIBDACUXLXVYRWVQMGWDRYRSQIUUFDJUBPGGDVZHDBKQBMNWSSWLFGXMGANDUWKAWLEIHMOLNNAXKDPHNULCGFTSYYZNCOAZJRPDRYXHIQWAPLVBGQULAZPUTJSCISWPGMMUKEMYFWCKCWXCJOPQDJBPUWVYGDVMQAXAXNACQNRHOGHCCKJSSBTMPUBFWLEWNWJSDYCCKFAVUHDYYGNYADBPTFQMXJZPETHVSYSLHSTOBWGKDYEPRFNLZJEGOKJMXDZBXRLGTGHOAIDZVTJZQPQAEHKHRBFDLOCDQOAJEYACHXRXFMFYUODNVPIJZNYVIFHZWHRJBQKGTQANDUYZTGRRFQVQMLNJDJEXSTLVIMEPYPHEZMYYKHXGSCZBSRDHXHMEGFCOTHCDVETICYWEDVNXJGJLAAOQLANLGPEUUSIKQMWXSTYHOPEG"

{- Function that will print "No result!" if Maybe type contains Nothing, or the
 - contents of the "Just" part otherwise. -}
printMaybe :: (Show a) => Maybe a -> IO ()
printMaybe = maybe (putStrLn "No result!") print

main = do
    putStrLn "First a test of encodeMessage: "
    print (encodeMessage "Here is a test input string." enigma1)
    putStrLn "And another test of encodeMessage: "
    print (encodeMessage "Here is a test input string." enigma2)
    putStrLn "Then a test of longestMenu: "
    print (longestMenu (zip x1 p1))
    putStrLn "Or maybe this is the one you should get: "
    print (longestMenu (zip p1 x1))
    putStrLn "And now SIX tests of breakEnigma:"
    putStrLn "NOTE: All but one of these most likely will return \"No result!\" after a very long search."
    putStrLn "You should get a result for at least one of them though."
   
    printMaybe (breakEnigma (zip p2 x2a))
    printMaybe (breakEnigma (zip p2 x2b))
    printMaybe (breakEnigma (zip p2 x2c))
    printMaybe (breakEnigma (zip x2a p2))
    printMaybe (breakEnigma (zip x2b p2))
    printMaybe (breakEnigma (zip x2c p2))
    
