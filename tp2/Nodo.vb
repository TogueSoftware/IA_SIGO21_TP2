Public Class Nodo
    Public _padre As Integer
    Public _identificador As Integer
    Public exitoso As Boolean = False
    Public _nivel As Integer = 0
    Public _sentido As Boolean = True
    Private _tolerancia As Double
    Public _inicial As Double

    Sub New()
        _padre = -1
        _identificador = -1
        exitoso = False
        _nivel = -1
        _inicial = -1
        _tolerancia = -1
    End Sub

    Sub New(pad As Integer, iden As Integer, tolerancia As Double, sentido As Boolean, inicial As Integer, profundidad As Integer)
        _padre = pad
        _identificador = iden
        _sentido = sentido
        _tolerancia = tolerancia
        _inicial = inicial
        _nivel = profundidad
        If Math.Abs(inicial) <= Math.Abs(tolerancia) Then
            'exitoso significa que el valor de posición está dentro de las tolerancias aceptadas por el brazo
            exitoso = True
        End If

        Dim s As String = " <<<< "
        If sentido Then
            s = " >>>> "
        End If
        Console.WriteLine("    Creando nodo id: " & _identificador.ToString("00") & " sentido: " & s & " nivel: " & _nivel.ToString("00") & " posición inicial: " & _inicial & " heuristica: " & heuristica())
    End Sub

    Public Sub cerrarNodo(nnn As Nodo)



        If cerradas(0) IsNot Nothing Then
            ReDim Preserve cerradas(cerradas.Length)
        End If



        cerradas(cerradas.Length - 1) = nnn
        Dim auxArr() As Nodo
        ReDim auxArr(0)

        Dim ii As Integer = 0
        For Each n As Nodo In abiertas
            If n IsNot Nothing Then
                If Not (n._identificador = nnn._identificador) Then
                    ReDim Preserve auxArr(ii)
                    auxArr(ii) = n
                    ii += 1
                End If
            End If

        Next

        If ii > 0 Then
            abiertas = Nothing
            ReDim abiertas(ii - 1)
            Array.Copy(auxArr, abiertas, abiertas.Length)
        Else
            Array.Clear(abiertas, 0, abiertas.Length)
        End If

    End Sub

    Public Sub ExpandirHeuristica()
        Dim niv As Integer = _nivel + 1

        'paso el nodo actual a la lista de cerrados
        Dim salir As Boolean = False

        Dim sent As Boolean = _sentido
        Dim aux_inicial As Double = siguiente(_sentido)
        Dim nuevos As Boolean = False
        cerrarNodo(Me)
        Dim aux_padre As Integer = _identificador

        Console.WriteLine(" Heuristica expandiendo nodo " & _identificador.ToString)
        sent = _sentido
        For ii = 1 To 2
            aux_inicial = Me.siguiente(sent)

            secuencia += 1
            If Not verificarExistencia(aux_inicial, sent) And Math.Abs(aux_inicial) < anchoBlock Then
                Dim nod = New Nodo(aux_padre, secuencia, _tolerancia, sent, aux_inicial, niv)
                ReDim Preserve abiertas(abiertas.Length)
                abiertas(abiertas.Length - 1) = nod
                nuevos = True
                'nodo no  existente, lo agrego a abiertas
                If nod.exitoso Then
                    'solucion encontrada.
                    Console.WriteLine("")
                    Console.WriteLine(" Solución heurística encontrada en el nodo " & nod._identificador & " del nivel " & nod._nivel)
                    Console.WriteLine("")
                    motrarSolucion(nod)
                    exito = True
                    Exit For
                End If
            End If
            sent = Not sent
        Next ii

        If Not nuevos Then
            Console.WriteLine("    Los nodos ya fueron procesados con anterioridad")
        End If

        If Not exito Then
            Try
                'buscando nodo con mejor heurística
                Dim nn As Nodo = buscaNodoMejorHeuristica()
                If nn._identificador > -1 Then
                    niv = nn._nivel + 1
                    aux_inicial = nn.siguiente(nn._sentido)
                    aux_padre = nn._identificador
                    sent = True
                    If niv <= maxNiveles And Math.Abs(aux_inicial) < anchoBlock Then
                        nn.cerrarNodo(nn)
                        nn.ExpandirHeuristica()
                    Else
                        fallido = True
                    End If
                Else
                    fallido = True
                End If
            Catch ex As Exception
                Console.WriteLine(" Error " & ex.ToString)
            End Try
        End If


    End Sub


    Public Sub ExpandirExploratoria()
        'paso el nodo actual a la lista de cerrados
        Dim salir As Boolean = False
        Dim niv As Integer = _nivel + 1
        Dim sent As Boolean = _sentido
        Dim aux_inicial As Double = siguiente(sent)
        Dim aux_padre_iz As Integer = _identificador
        Dim aux_padre_der As Integer = _identificador
        Dim aux_padre As Integer = _identificador
        Dim aux_max_der As Integer = _inicial
        Dim aux_max_iz As Integer = _inicial
        Console.WriteLine(" Exploratoria expandiendo nodo " & _identificador.ToString)
        Do
            secuencia += 1

            Dim nod = New Nodo(aux_padre, secuencia, _tolerancia, sent, aux_inicial, niv)
            aux_inicial = nod.siguiente(sent)
            aux_padre = secuencia
            If Not verificarExistencia(nod._inicial, sent) Then
                If sent Then
                    aux_max_der = aux_inicial
                    aux_padre_der = aux_padre
                Else
                    aux_max_iz = aux_inicial
                    aux_padre_iz = aux_padre
                End If
                ReDim Preserve abiertas(abiertas.Length)
                abiertas(abiertas.Length - 1) = nod
                'nodo no  existente, lo agrego a abiertas

                If nod.exitoso Then
                    'solucion encontrada.
                    Console.WriteLine("")
                    Console.WriteLine("Solución exploratoria encontrada en el nodo " & nod._identificador & " del nivel " & nod._nivel)
                    Console.WriteLine("")
                    motrarSolucion(nod)
                    exito = True
                    salir = True
                End If
            End If

            If Not salir Then
                If niv = maxNiveles Or (Math.Abs(aux_inicial) > anchoBlock) Then
                    If sent Then
                        'ultima pasada hacia la derecha
                        aux_inicial = aux_max_iz
                        aux_padre = aux_padre_iz
                        sent = False
                    Else
                        'intento por izquierda, no fue encontrado
                        salir = True
                        If niv = maxNiveles Then
                            fallido = True
                        End If

                    End If
                Else
                    If sent Then
                        'siguiente pasada hacia la derecha
                        aux_inicial = aux_max_iz
                        aux_padre = aux_padre_iz
                    Else
                        'hacia la derecha en el siguiente nivel
                        niv += 1
                        aux_inicial = aux_max_der
                        aux_padre = aux_padre_der
                    End If
                    Console.WriteLine(" Exploratoria cambiando nivel y ampliando distancia")
                    sent = Not sent
                End If
            End If
        Loop Until salir Or exito

    End Sub

    Private Function buscaNodo(identificador As Integer) As Nodo
        Dim respuesta As Nodo = New Nodo()
        Dim encontrado As Boolean = False
        Dim n As Nodo

        For ii = 0 To abiertas.Length - 1
            n = abiertas(ii)
            If n IsNot Nothing Then
                If n._identificador = identificador Then
                    respuesta = n
                    encontrado = True
                End If
            End If
        Next

        If Not encontrado Then
            For ii = 0 To cerradas.Length - 1
                n = cerradas(ii)
                If n IsNot Nothing Then
                    If n._identificador = identificador Then
                        respuesta = n
                        encontrado = True
                    End If
                End If
            Next
        End If

        Return respuesta
    End Function

    Private Function buscaNodoMejorHeuristica() As Nodo
        Dim respuesta As Nodo = New Nodo
        Dim aux_heu As Double = 999999999999999999

        For Each nn As Nodo In abiertas
            If nn IsNot Nothing Then
                If aux_heu > nn.heuristica Then
                    If nn._padre > -2 Then
                        aux_heu = nn.heuristica
                        respuesta = nn
                    End If
                End If
            End If
        Next
        Return respuesta
    End Function

    Private Sub motrarSolucion(n As Nodo)
        Dim i As Integer = 0
        Dim cant As Integer = n._identificador
        Dim cad As String = "   Meta !!!!! --"
        Dim s As String
        Dim p As String
        Dim euristica As Double = 0
        Do
            If i > 0 Then
                cad = "   BackTrack " & i.ToString & "  "
            End If
            If n._sentido Then
                s = " >>>> "
            Else
                s = " <<<< "
            End If
            If n._padre > -2 Then
                p = "  padre " & n._padre.ToString("000") & " heuristica " & n.heuristica()
            Else
                p = "  nodo raiz "
                s = " |||| "
            End If
            euristica += n.heuristica()
            Console.WriteLine(cad & "-> Nodo ID " & n._identificador.ToString("00") & " sentido " & s & " nivel " & n._nivel.ToString("000") & p)
            i += 1
            n = buscaNodo(n._padre)
        Loop Until n._padre = -1

        Console.WriteLine("")
        Console.WriteLine("   Se exploraron " & cant.ToString & " nodos y se necesitan " & (i - 1).ToString & " Pasos hacia la solución")
        Console.WriteLine("")
        Console.WriteLine("   Eurística acumulada " & euristica)
        Console.WriteLine("")
    End Sub


    Public Function heuristica() As Double
        Return _nivel * anchoBlock + Math.Abs(_inicial)
    End Function


    Public Function siguiente(ByVal sentido As Boolean) As Double

        Dim h As Double = _tolerancia * 5
        If sentido Then
            h = _inicial + (_tolerancia * 1.5)
        Else
            h = _inicial - (_tolerancia * 1.5)
        End If

        Return h
    End Function
End Class
