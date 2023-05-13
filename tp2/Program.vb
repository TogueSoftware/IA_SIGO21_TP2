Imports System

Module Program

    ' Se asumen los siguientes estados:

    ' la posición inicial del brazo está definida por la variable posicionX (positiva hacia la derecha, negativa hacia la izquierda).
    ' la tolerancia de posicion en cual el brazo puede trabajar está definida por la variable toleranciaX (segón consigna).
    ' la cantidad máxima de niveles a explorar está definida por la variable maxNiveles.
    ' el tamaño máximo del block del motor está definido por la variable anchoBlock.
    ' el desplazamiento en cada paso se calcula en un 150% de la tolerancia para reducir la cantidad de nodos explorados.
    ' para el calculo de la función heurística, se toma la distancia desde el inicio multiplicando el nivel * en ancho máximo del motor,
    '   esto garantiza que los valores de posición no van a solapar a los de nivel
    ' todas las medidas se encuentran en mm


    'mapa de estados (posiciones iniciales)
    Dim posicionX As Integer = -288

    'tamaño máximo de desplazamiento debido al tamaño de la pieza del block
    Public anchoBlock As Integer = 800

    'tolerancia aceptada por el brazo de robot para colocar la pieza
    Dim toleranciaX As Double = 12 ' eje horizontal

    'profundidad máxima de exploración (niveles)
    Public maxNiveles As Integer = 50

    'secuenciador de numero de nodos
    Public secuencia As Integer

    'Array de nodos abiertos y cerrados
    Public abiertas() As Nodo
    Public cerradas() As Nodo

    'condición de solución encontrada
    Public exito As Boolean = False
    Public fallido As Boolean = False

    Dim d As Nodo
    Sub Main(args As String())
        Console.WriteLine("inicio de programa")
        Dim fallido As Boolean = False
        Dim sentido As Boolean = True
        sentido = True
        fallido = False
        exito = False

        For ii = 1 To 2
            fallido = False
            secuencia = 1
            exito = False
            Console.WriteLine("")
            Console.WriteLine("--------------------------------------------------------")
            If ii = 1 Then
                Console.WriteLine("           Método Exploratorio")
                Console.WriteLine("                     |->")
                Console.WriteLine("                   <-|")
                Console.WriteLine("                     |-->")
                Console.WriteLine("                  <--|")
                Console.WriteLine("                     |--->")
                Console.WriteLine("               X <---|")
                Console.WriteLine("")
                Console.WriteLine("           Va expandiendo los límites sobre el eje")
                Console.WriteLine("")

            Else
                Console.WriteLine("       Método Heurístico con mejor solución")
                Console.WriteLine("                     |->")
                Console.WriteLine("                     |-->")
                Console.WriteLine("                   <-|")
                Console.WriteLine("                     |--->")
                Console.WriteLine("                  <--|")
                Console.WriteLine("               X <---|")
                Console.WriteLine("")
                Console.WriteLine("   Expande el nodo sin procesar que tiene menor peso")
                Console.WriteLine("")
            End If
            Console.WriteLine("--------------------------------------------------------")
            Console.WriteLine("")
            Console.WriteLine("")
            Console.WriteLine("                 Analizando Eje X")
            Console.WriteLine("")
            d = New Nodo(-2, secuencia, toleranciaX, sentido, posicionX, 0)
            ReDim abiertas(0)
            ReDim cerradas(0)
            abiertas(0) = d
            Do
                Console.WriteLine("")
                If (abiertas.Count = 0) Then

                    fallido = True
                Else
                    'tomo el primer elemento de la lista de abiertas y lo expando
                    'esto asegura que voy a expandir en una cola tipo FIFO, pero despues de haber explorado la profundidad
                    Try
                        If abiertas(0) IsNot Nothing Then
                            If Not abiertas(0).exitoso Then
                                If ii = 1 Then
                                    abiertas(0).ExpandirExploratoria()

                                Else
                                    abiertas(0).ExpandirHeuristica()
                                End If
                                fallido = Not exito
                            Else
                                'encontrada la solución en el primer nodo no se requiere expandir
                                Console.WriteLine("Ya se encuentra sobre la solución, no se requiere análisis...")
                                exito = True
                            End If
                        End If
                    Catch
                        fallido = True
                    End Try

                End If
                'exito viene de dentro de los nodos, fallido cuando no quedan nodos en la lista de abiertas
            Loop Until exito Or fallido


            If fallido Then
                Console.WriteLine("")
                Console.WriteLine("  Búsqueda fallida...")
            End If
        Next
    End Sub


    Public Function verificarExistencia(ByVal inicial As Double, ByVal sentido As Double) As Boolean
        Dim respuesta As Boolean = False
        Dim n As Nodo

        For ii = 0 To abiertas.Length - 1
            n = abiertas(ii)
            If n IsNot Nothing Then
                If n._inicial = inicial Then
                    If n._sentido = sentido Then
                        respuesta = True
                        Exit For
                    End If
                End If
            End If
        Next

        If Not respuesta Then
            For ii = 0 To cerradas.Length - 1
                n = cerradas(ii)
                If n IsNot Nothing Then
                    If n._inicial = inicial Then
                        If n._sentido = sentido Then
                            respuesta = True
                            Exit For
                        End If
                    End If
                End If
            Next
        End If
        Return respuesta
    End Function
End Module

