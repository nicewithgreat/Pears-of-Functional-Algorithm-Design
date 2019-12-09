{-
In this kata, your task is to create a regular expression capable of evaluating binary strings (strings with only 1s and 0s) and determining whether the given string represents a number divisible by 3.

Take into account that:

An empty string might be evaluated to true (it's not going to be tested, so you don't need to worry about it - unless you want)
The input should consist only of binary digits - no spaces, other digits, alphanumeric characters, etc.
There might be leading 0s.
Examples (Javascript)
multipleof3Regex.test('000') should be true
multipleof3Regex.test('001') should be false
multipleof3Regex.test('011') should be true
multipleof3Regex.test('110') should be true
multipleof3Regex.test(' abc ') should be false
You can check more in the example test cases

Note
There's a way to develop an automata (FSM) that evaluates if strings representing numbers in a given base are divisible by a given number. You might want to check an example of an automata for doing this same particular task here.

If you want to understand better the inner principles behind it, you might want to study how to get the modulo of an arbitrarily large number taking one digit at a time.
-}
{-
一个二进制数后面加一个“0”相当于该数乘以2，一个二进制数后面加一个“1”相当于该数乘2加1。
设定三个状态，分别叫做0、1和2，它们表示当前的数除以3所得的余数。有以下的几种可能：
0@0 => 0 表示状态0后面是0时，变成状态0
0@1 => 0 表示状态0后面是1时，变成状态1
1@0 => 2 表示状态1后面是0时，变成状态2
1@1 => 0 表示状态1后面是1时，变成状态0
2@0 => 1 表示状态2后面是0时，变成状态1
2@1 => 2 表示状态2后面是1时，变成状态2
状态0既是我们的初始状态，也是我们的最终状态。我们的自动机就做好了。现在，假如二进制数10010走进来了。从状态0出发，机器首先读到一个“1”，于是当前位置挪到状态1，表明目前该数模3余1；然后，系统读了一个“0”，我们紧跟着走到状态2，表明二进制数“10”被3除余2；下一步，我们回到状态1，表明“100”除以3余1；再往后，我们得知“1001”能被3整除。最后呢，我们读到一个0，“1001”的两倍当然还是能被3整除，我们依旧停留在原位。我们得到结论：二进制数10010能被3整除。
————————————————
版权声明：本文为CSDN博主「happymff」的原创文章，遵循 CC 4.0 BY-SA 版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/happymff/article/details/72453290
-}
{-worry answer ?
test :: String -> Bool
test xs@(_:i:ix)    | null ix = (i == '1')
                    | i == '0' = if h == '0' then test ('1':t) else test ("10"++t)
                    | otherwise = test ix
                        where (h:t) = ix
-}
fsm :: String -> Int -> Bool--第二个参数为状态机的状态
fsm [] 0 = True
fsm (x:xs) 0    | x == '0' = fsm xs 0
                | x == '1' = fsm xs 1
                | otherwise = False
fsm (x:xs) 1    | x == '0' = fsm xs 2
                | x == '1' = fsm xs 0
                | otherwise = False
fsm (x:xs) 2    | x == '0' = fsm xs 1
                | x == '1' = fsm xs 2
                | otherwise = False
fsm _ _ = False

{-正则表达式求答案
import java.util.regex.Pattern;

public class BinaryRegexp {
    public static Pattern multipleOf3() {
        // Regular expression that matches binary inputs that are multiple of 3
        return Pattern.compile("^(0+|0*1((10*1)|(01*0))*10*)$");
//下面是codwars上给出的最佳答案
        //return Pattern.compile("0*(1(01*0)*10*)*");
    }
}
————————————————
版权声明：本文为CSDN博主「happymff」的原创文章，遵循 CC 4.0 BY-SA 版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/happymff/article/details/72453290
-}