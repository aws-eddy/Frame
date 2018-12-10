namespace FrameProjectTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FrameParser
open Parser

[<TestClass>]
type ParserTestClass () = 
    [<TestMethod>]
    member this.CheckParser() = 
        let input = """fr(ht("Hello"), ht(" \" ; /$#!@&*-_{}|+ World"));"""
        let expected = "fr(ht(\"Hello\"),ht(\" $0$ $1$ /$2$#!@&*-_{}|+ World\"));"
        let result = stripSpacesSelectively input
        Assert.AreEqual(expected, result)
[<TestClass>]
type StructTestClass () = 
    
    [<TestMethod>]
    member this.CheckFrameOfFrame() =
        let input = "fr(fr(ht(\"Hello World\")));"
        let expected = "\n\t<div>\n\t\t<div>\n\t\t\t<h1>\n\t\t\t\tHello World\n\t\t\t</h1>\n\t\t</div>\n\t</div>"
        let result = parse input
        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.CheckListOfText() = 
        let input = "ht(\"str1\",\"str2\",\"str3\");"
        let expected = "\n\t<h1>\n\t\tstr1\n\t</h1>\n\t<h1>\n\t\tstr2\n\t</h1>\n\t<h1>\n\t\tstr3\n\t</h1>"
        let result = parse input
        Assert.AreEqual(expected,result)

[<TestClass>]
type VariableCheckClass() = 
    [<TestMethod>]
    member this.CheckAssignmentOp() =
        let input = "x = fr(\"\");\nx;"
        let expected = "\n\t<div>\n\t\t\n\t</div>"
        let result = parse input
        Assert.AreEqual(expected,result)

    [<TestMethod>]
        member this.CheckStringAssignOp() =
            let input = "x = \"Hello string\";\n x;"
            let expected = "\n\tHello string"
            let result = parse input
            Assert.AreEqual(expected,result)
    [<TestMethod>]
        member this.CheckNestedVariables() =
            let input = "x = ht(\"Head Text\");\ny = fr(x);\ny;"
            let expected = "\n\t<div>\n\t\t<h1>\n\t\t\tHead Text\n\t\t</h1>\n\t</div>"
            let result = parse input
            Assert.AreEqual(expected,result)

[<TestClass>]
type TextTestClass() = 

    [<TestMethod>]
    member this.ReturnHeadText() =
        let input = "ht(\"hello world\");"
        let expected = "\n\t<h1>\n\t\thello world\n\t</h1>"
        let result = parse input
        Assert.AreEqual(expected,result)

    [<TestMethod>]
    member this.ReturnParaText() =
        let input = "pt(\"paragraph\");"
        let expected = "\n\t<p>\n\t\tparagraph\n\t</p>"
        let result = parse input
        Assert.AreEqual(expected,result)
  
    [<TestMethod>]
    member this.ReturnOrderedList() =
        let input = "ol(\"Ordered List\");"
        let expected = "\n\t<ol>\n\t\tOrdered List\n\t</ol>"
        let result = parse input
        Assert.AreEqual(expected,result)
     
    [<TestMethod>]
    member this.ReturnBoldedText() =
        let input = "bt(\"Bold Text\");"
        let expected = "\n\t<b>\n\t\tBold Text\n\t</b>"
        let result = parse input
        Assert.AreEqual(expected,result)    