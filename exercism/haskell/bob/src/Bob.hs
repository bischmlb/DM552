module Bob (responseFor) where

--Bob is a lackadaisical teenager. In conversation, his responses are very limited.
--Bob answers 'Sure.' if you ask him a question, such as "How are you?".
--He answers 'Whoa, chill out!' if you YELL AT HIM (in all capitals).
--He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
--He says 'Fine. Be that way!' if you address him without actually saying anything.
--He answers 'Whatever.' to anything else.
--Bob's conversational partner is a purist when it comes to written communication and always follows normal rules regarding sentence punctuation in English.

bonusChars = ['1','2','3','4','5','6','7','8','9','0'] ++ [' ','!',',','%','^','*','@','#','$','(','*','^',']']

responseFor :: String -> String
responseFor "" = "Fine. Be that way!"
responseFor xs
  | checkIfQuestionMark xs = "Sure."
  | checkIfAllCapsNoQ xs = "Whoa, chill out!"
  | checkIfAllCapsAndQ xs = "Calm down, I know what I'm doing!"
  | checkIfBonusChar xs = "Whatever"
  | otherwise = "Whatever."

checkIfBonusChar :: String -> Bool
checkIfBonusChar text = all (\x -> elem x bonusChars) text

checkIfQuestionMark :: String -> Bool
checkIfQuestionMark text
  | (\x -> elem x text) '?' && checkIfAllCapsAndQ text == False = True
  | (\x -> elem x text) '?' && length text < 2 = True
  | otherwise = False

checkIfAllCapsNoQ :: String -> Bool
checkIfAllCapsNoQ text
  | all (\x -> elem x (['A'..'Z'] ++ bonusChars)) text && checkIfQuestionMark text == False = True
  | otherwise = False


checkIfAllCapsAndQ :: String -> Bool
checkIfAllCapsAndQ text = all (\x -> elem x (['A'..'Z'] ++ [' '] ++ ['?','!'])) text && length text > 1
