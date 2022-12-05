Public Class Form1
    Public Matrix(9, 70) As String
    Public BoxAntal(9) As Integer
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim FileName As String
        Dim TextRad As String
        'TextBox1.Text = "E:\temp\gstegar.txt"
        FileName = "J:\Program\Adventofcode\2022\Input5.txt"
        Dim AntalRader As Integer
        Dim Rad As Integer
        Dim TotalSumma As Integer
        'Global Matrix(9, 70) As String
        Dim Kol As Integer
        Dim Box As Integer
        Dim Antal As Integer
        Dim From As Integer
        Dim Till As Integer
        Dim Varde() As String
        AntalRader = 0
        Rad = 0
        TotalSumma = 0


        ' antal rader i texten
        AntalRader = System.IO.File.ReadAllLines(FileName).Length
        Dim ObjReader As New System.IO.StreamReader(FileName)
        Dim File As New System.IO.StreamWriter("J:\Program\Adventofcode\2022\Output3.txt")
        Stop

        Do While ObjReader.Peek() <> -1
            TextRad = ObjReader.ReadLine()
            ' läs in boxar
            For x = 8 To 1 Step -1
                Matrix(1, x) = Mid(TextRad, 2, 1)
                Matrix(2, x) = Mid(TextRad, 6, 1)
                Matrix(3, x) = Mid(TextRad, 10, 1)
                Matrix(4, x) = Mid(TextRad, 14, 1)
                Matrix(5, x) = Mid(TextRad, 18, 1)
                Matrix(6, x) = Mid(TextRad, 22, 1)
                Matrix(7, x) = Mid(TextRad, 26, 1)
                Matrix(8, x) = Mid(TextRad, 30, 1)
                Matrix(9, x) = Mid(TextRad, 34, 1)
                TextRad = ObjReader.ReadLine()
            Next
            Exit Do
        Loop
        ' Sätter var sista boxen är
        For x = 1 To 9
            For y = 1 To 9
                If Matrix(x, y) = " " Or Matrix(x, y) = Nothing Then
                    BoxAntal(x) = y - 1
                    Exit For
                End If
            Next
        Next



        Dim ObjReader2 As New System.IO.StreamReader(FileName)
        ' läs in förflyttningar
        Do While ObjReader.Peek() <> -1
            TextRad = ObjReader.ReadLine()
            If Mid(TextRad, 1, 1) = "m" Then
                Varde = TextRad.Split(" ")
                Antal = Varde(1)
                From = Varde(3)
                Till = Varde(5)
                'For x = 1 To Antal
                'Flytt(From, Till)
                'Next
                Flytt2(Antal, From, Till)
            End If
        Loop

        TextBox1.Text = ""
        For x = 1 To 9
            TextBox1.Text = TextBox1.Text + Matrix(x, BoxAntal(x))

        Next

        Label2.Text = "Klart"

        File.Close()
    End Sub
    Function Flytt2(ByVal A As Integer, ByVal F As Integer, ByVal T As Integer)

        Dim Varde(A) As String
        For x = 1 To A
            Varde(x) = Matrix(F, BoxAntal(F))
            Matrix(F, BoxAntal(F)) = Nothing
            BoxAntal(F) = BoxAntal(F) - 1
        Next
        For x = A To 1 Step -1
            BoxAntal(T) = BoxAntal(T) + 1
            Matrix(T, BoxAntal(T)) = Varde(x)
        Next


    End Function
    Function Flytt(ByVal F As Integer, ByVal T As Integer)

        Dim Varde As String = Matrix(F, BoxAntal(F))
        Matrix(F, BoxAntal(F)) = Nothing
        BoxAntal(F) = BoxAntal(F) - 1
        BoxAntal(T) = BoxAntal(T) + 1
        Matrix(T, BoxAntal(T)) = Varde

    End Function
    Function Check_Same_letter(ByVal P1 As String, ByVal P2 As String, ByVal P3 As String) As String
        For X = 1 To Len(P1)
            If Check_L(Mid(P1, X, 1), P2) = True Then
                If Check_L(Mid(P1, X, 1), P3) = True Then
                    ' Finns i alla tre
                    Check_Same_letter = Mid(P1, X, 1)
                    Exit Function
                End If
            End If
        Next
    End Function
    Function Letter(ByVal P1 As String, ByVal P2 As String) As String
        For X = 1 To Len(P1)
            If Check_L(Mid(P1, X, 1), P2) = True Then
                Letter = Mid(P1, X, 1)
                Exit Function
            End If
        Next
    End Function
    Function Check_L(ByVal L As String, ByVal LString As String) As Boolean
        If InStr(LString, L) > 0 Then
            Check_L = True
        Else
            Check_L = False
        End If

    End Function

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        End
    End Sub
    Private Sub Uppgift1()
        Dim FileName As String
        Dim TextRad As String
        'TextBox1.Text = "E:\temp\gstegar.txt"
        FileName = "J:\Program\Adventofcode\2022\Input1.txt"
        Dim AntalRader As Integer
        Dim Rad As Integer
        Dim Elf As Integer
        Dim TopElfK As Integer
        Dim Summa As Integer
        AntalRader = 0
        Rad = 0
        Elf = 0
        TopElfK = 0
        Summa = 0

        ' antal rader i texten
        AntalRader = System.IO.File.ReadAllLines(FileName).Length
        Dim ObjReader As New System.IO.StreamReader(FileName)
        Dim File As New System.IO.StreamWriter("J:\Program\Adventofcode\2022\Output1.txt")

        Do While ObjReader.Peek() <> -1
            TextRad = ObjReader.ReadLine()
            'summera K tills slut

            If TextRad = "" Then
                File.WriteLine(Summa)
                If Summa > TopElfK Then TopElfK = Summa
                Summa = 0
            Else
                Summa = Summa + Int(TextRad)
            End If

            Rad = Rad + 1
        Loop

        Label2.Text = "Klart"
        Label1.Text = TopElfK
        File.Close()

    End Sub
    Private Sub Uppgift2()
        Dim FileName As String
        Dim TextRad As String
        'TextBox1.Text = "E:\temp\gstegar.txt"
        FileName = "J:\Program\Adventofcode\2022\Input2.txt"
        Dim AntalRader As Integer
        Dim Rad As Integer
        Dim TotalSumma As Integer
        Dim TopElfK As Integer
        Dim Summa As Integer
        AntalRader = 0
        Rad = 0
        Dim Val As String
        Val = ""
        TopElfK = 0
        Summa = 0
        TotalSumma = 0

        ' antal rader i texten
        AntalRader = System.IO.File.ReadAllLines(FileName).Length
        Dim ObjReader As New System.IO.StreamReader(FileName)
        Dim File As New System.IO.StreamWriter("J:\Program\Adventofcode\2022\Output1.txt")
        Stop
        ' Sten, papper, sax
        Do While ObjReader.Peek() <> -1
            TextRad = ObjReader.ReadLine()
            Val = Mid(TextRad, 1, 1)
            Select Case Mid(TextRad, 3, 1)
                Case "X" 'Förlora
                    If Val = "A" Then
                        Summa = 0 + 3
                    End If
                    If Val = "B" Then
                        Summa = 0 + 1
                    End If
                    If Val = "C" Then
                        Summa = 0 + 2
                    End If
                Case "Y" ' oavgjort
                    If Val = "A" Then
                        Summa = 3 + 1
                    End If
                    If Val = "B" Then
                        Summa = 3 + 2
                    End If
                    If Val = "C" Then
                        Summa = 3 + 3
                    End If
                Case "Z" ' vinn
                    If Val = "A" Then
                        Summa = 6 + 2
                    End If
                    If Val = "B" Then
                        Summa = 6 + 3
                    End If
                    If Val = "C" Then
                        Summa = 6 + 1
                    End If
            End Select
            TotalSumma = TotalSumma + Summa
            Summa = 0
            'File.WriteLine(Summa)

            Rad = Rad + 1
        Loop

        Label2.Text = "Klart"
        Label1.Text = TotalSumma
        File.Close()
    End Sub
    Private Sub Uppgift3_part_1()
        Dim FileName As String
        Dim TextRad As String
        'TextBox1.Text = "E:\temp\gstegar.txt"
        FileName = "J:\Program\Adventofcode\2022\Input3.txt"
        Dim AntalRader As Integer
        Dim Rad As Integer
        Dim TotalSumma As Integer
        Dim Part1 As String
        Dim Part2 As String
        Dim Bok As String
        AntalRader = 0
        Rad = 0
        TotalSumma = 0

        ' antal rader i texten
        AntalRader = System.IO.File.ReadAllLines(FileName).Length
        Dim ObjReader As New System.IO.StreamReader(FileName)
        Dim File As New System.IO.StreamWriter("J:\Program\Adventofcode\2022\Output3.txt")
        Stop
        ' Dela på texten i 2 delar
        Do While ObjReader.Peek() <> -1
            TextRad = ObjReader.ReadLine()
            Part1 = Mid(TextRad, 1, Len(TextRad) / 2)
            Part2 = Mid(TextRad, Len(TextRad) / 2 + 1, Len(TextRad))
            Bok = Letter(Part1, Part2)
            'File.WriteLine(Bok)
            Rad = Rad + 1
            'Label1.Text = Asc("Q")
            'Bok = "a"
            If Asc(Bok) > 96 Then
                TotalSumma = TotalSumma + Asc(Bok) - 96
            Else
                TotalSumma = TotalSumma + Asc(Bok) - 38
            End If
            'TotalSumma = TotalSumma + Asc(Bok)
            ' a-z = 97 to 122
            ' A-Z = 65 to 90

        Loop
        TextBox1.Text = TotalSumma
        Label2.Text = "Klart"

        File.Close()

    End Sub
    Private Sub Uppgift3_part_2()
        Dim FileName As String
        Dim TextRad As String
        'TextBox1.Text = "E:\temp\gstegar.txt"
        FileName = "J:\Program\Adventofcode\2022\Input3.txt"
        Dim AntalRader As Integer
        Dim Rad As Integer
        Dim TotalSumma As Integer
        Dim Part1 As String
        Dim Part2 As String
        Dim Part3 As String
        Dim Bok As String
        AntalRader = 0
        Rad = 0
        TotalSumma = 0

        ' antal rader i texten
        AntalRader = System.IO.File.ReadAllLines(FileName).Length
        Dim ObjReader As New System.IO.StreamReader(FileName)
        Dim File As New System.IO.StreamWriter("J:\Program\Adventofcode\2022\Output3.txt")
        Stop

        Do While ObjReader.Peek() <> -1
            Part1 = ObjReader.ReadLine()
            Part2 = ObjReader.ReadLine()
            Part3 = ObjReader.ReadLine()
            ' Leta efter gemensam bokstav
            Bok = Check_Same_letter(Part1, Part2, Part3)

            'Räkna antal av den bokstaven
            If Asc(Bok) > 96 Then
                TotalSumma = TotalSumma + Asc(Bok) - 96
            Else
                TotalSumma = TotalSumma + Asc(Bok) - 38
            End If

            'For Each c As Char In Part1 + Part2 + Part3
            'If c = Bok Then TotalSumma = TotalSumma + 1
            'Next

            'TotalSumma = TotalSumma + Asc(Bok)

        Loop
        TextBox1.Text = TotalSumma
        Label2.Text = "Klart"

        File.Close()
    End Sub
    Private Sub Uppgift4()
        Dim FileName As String
        Dim TextRad As String
        'TextBox1.Text = "E:\temp\gstegar.txt"
        FileName = "J:\Program\Adventofcode\2022\Input4.txt"
        Dim AntalRader As Integer
        Dim Rad As Integer
        Dim TotalSumma As Integer
        Dim Max1 As Integer
        Dim Min1 As Integer
        Dim Max2 As Integer
        Dim Min2 As Integer
        Dim Komma As Integer
        Dim Bind1 As Integer
        Dim Bind2 As Integer
        Dim Part1 As String
        Dim Part2 As String
        Dim Part3 As String
        Dim Bok As String
        AntalRader = 0
        Rad = 0
        TotalSumma = 0

        ' antal rader i texten
        AntalRader = System.IO.File.ReadAllLines(FileName).Length
        Dim ObjReader As New System.IO.StreamReader(FileName)
        Dim File As New System.IO.StreamWriter("J:\Program\Adventofcode\2022\Output3.txt")
        Stop

        Do While ObjReader.Peek() <> -1
            TextRad = ObjReader.ReadLine()
            ' ta ut min och max värden
            Komma = InStr(TextRad, ",")
            Bind1 = InStr(TextRad, "-")
            Bind2 = InStr(Komma, TextRad, "-")

            Min1 = Mid(TextRad, 1, Bind1 - 1)
            Max1 = Mid(TextRad, Bind1 + 1, Komma - Bind1 - 1)
            Min2 = Mid(TextRad, Komma + 1, Bind2 - Komma - 1)
            Max2 = Mid(TextRad, Bind2 + 1, Len(TextRad) - Bind2)
            ' se om en sekvens finns i den andra
            If Min1 >= Min2 And Max1 <= Max2 Then
                TotalSumma = TotalSumma + 1
            ElseIf Min2 >= Min1 And Max2 <= Max1 Then
                TotalSumma = TotalSumma + 1
            ElseIf Max1 >= Min2 And Max1 <= Max2 Then
                TotalSumma = TotalSumma + 1
            ElseIf Min1 >= Min2 And Min1 <= Max2 Then
                TotalSumma = TotalSumma + 1
            ElseIf Max2 >= Min1 And Max2 <= Max1 Then
                TotalSumma = TotalSumma + 1
            ElseIf Min2 >= Min1 And Min2 <= Max1 Then
                TotalSumma = TotalSumma + 1
            End If


        Loop
        TextBox1.Text = TotalSumma
        Label2.Text = "Klart"

        File.Close()
    End Sub
    Private Sub Uppgift5()
        Dim FileName As String
        Dim TextRad As String
        'TextBox1.Text = "E:\temp\gstegar.txt"
        FileName = "J:\Program\Adventofcode\2022\Input5.txt"
        Dim AntalRader As Integer
        Dim Rad As Integer
        Dim TotalSumma As Integer
        'Global Matrix(9, 70) As String
        Dim Kol As Integer
        Dim Box As Integer
        Dim Antal As Integer
        Dim From As Integer
        Dim Till As Integer
        Dim Varde() As String
        AntalRader = 0
        Rad = 0
        TotalSumma = 0

        ' antal rader i texten
        AntalRader = System.IO.File.ReadAllLines(FileName).Length
        Dim ObjReader As New System.IO.StreamReader(FileName)
        Dim File As New System.IO.StreamWriter("J:\Program\Adventofcode\2022\Output3.txt")
        Stop

        Do While ObjReader.Peek() <> -1
            TextRad = ObjReader.ReadLine()
            ' läs in boxar
            For x = 8 To 1 Step -1
                Matrix(1, x) = Mid(TextRad, 2, 1)
                Matrix(2, x) = Mid(TextRad, 6, 1)
                Matrix(3, x) = Mid(TextRad, 10, 1)
                Matrix(4, x) = Mid(TextRad, 14, 1)
                Matrix(5, x) = Mid(TextRad, 18, 1)
                Matrix(6, x) = Mid(TextRad, 22, 1)
                Matrix(7, x) = Mid(TextRad, 26, 1)
                Matrix(8, x) = Mid(TextRad, 30, 1)
                Matrix(9, x) = Mid(TextRad, 34, 1)
                TextRad = ObjReader.ReadLine()
            Next
            Exit Do
        Loop
        ' Sätter var sista boxen är
        For x = 1 To 9
            For y = 1 To 9
                If Matrix(x, y) = " " Or Matrix(x, y) = Nothing Then
                    BoxAntal(x) = y - 1
                    Exit For
                End If
            Next
        Next



        Dim ObjReader2 As New System.IO.StreamReader(FileName)
        ' läs in förflyttningar
        Do While ObjReader.Peek() <> -1
            TextRad = ObjReader.ReadLine()
            If Mid(TextRad, 1, 1) = "m" Then
                Varde = TextRad.Split(" ")
                Antal = Varde(1)
                From = Varde(3)
                Till = Varde(5)
                'For x = 1 To Antal
                'Flytt(From, Till)
                'Next
                Flytt2(Antal, From, Till)
            End If
        Loop

        TextBox1.Text = ""
        For x = 1 To 9
            TextBox1.Text = TextBox1.Text + Matrix(x, BoxAntal(x))

        Next

        Label2.Text = "Klart"

        File.Close()

    End Sub
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Button1.PerformClick()
    End Sub
End Class
