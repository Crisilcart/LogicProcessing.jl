module LogicProcessing

#################################################################################
# Creador: Juan David Zorrilla Herrergit config --global user.email "EMAIL"a y Cristhian Silvita Cartagena            #
# Fecha: 17/01/2018                                                             #
# Empresa: KNAR S.A.                                                            #
# Lenguaje: Julia 0.6.2                                                         #
# Titulo: Librerias.jl                                                          #
# Versión Actual: 2.0                                                           #
# Descripción: Este programa es la creacion de unas librerias necesarias para la#
# generación de arreglos de numeros aleatorios utilizando la CPU en donde ya te-#
# nemos la generacion de matrices aleatorias y una funcion logica (||) implemen-#
# tada                                                                          #
# Estado: TRABAJANDO EN LA SIGUIENTE git config --global user.email "EMAIL"VERSIÓN                                    #
#################################################################################
## Variables Globales
global Mat=Array[], num=0, ID=Array[],Res=Array[]
################################################################################
export Evento,LogicOpMat,DispMat
using Distributions
using PyPlot

# FUNCIÓN QUE ME RETORNA UN NUMERO ALEATORIO VARIANDO SEGÚN LA FUNCIÓN DE DISTRIBUCIÓN
function probability_function(type_fc;μ=NaN,σ=NaN,α=NaN,θ=NaN)
  # Función con distribución de tipo Normal
  if type_fc=="normal"        # Tipo de función
    if isequal(μ,NaN)||isequal(σ,NaN) # Verifica que se hayan ingresado los parametros correctos
      println("Se requiere μ(Media) y σ(Desviación Estandar)")
      return NaN
    end
    return rand(Normal(μ,σ))  #Retorna el valor aleatorio
  end
  # Función con distribución de tipo Weibull
  if type_fc=="weibull" # Tipo de función
    if isequal(α,NaN)||isequal(θ,NaN) # Verifica que se hayan ingresado los parametros correctos
      println("Se requiere α y θ")
      return NaN
    end
    return rand(Weibull(α,θ)) #Retorna el valor aleatorio
  end
  # Función con distribución de tipo Exponencial
  if type_fc=="exponential" # Tipo de función
    if isequal(θ,NaN)       # Verifica que se hayan ingresado los parametros correctos
      println("Se requiere θ")
      return NaN
    end
    return rand(Exponential(θ)) #Retorna el valor aleatorio
  end
  # Error al ingresar los parametros
  println("Ingreso mal la función de probabilidad");
  return NaN
end
#FUNCION QUE ME RETORNA UNA MATRIZ DE VALORES QUE ME INDICAN LAS FALLAS DE UN EQUIPO
function VecAlea(t_final,fix,type_fc;miui=NaN,sigmai=NaN,alphai=NaN,thetai=NaN)
  timenow=0;  # Tiempo actual
  fail=[0.0]  #Vector FLOAT que indica las FALLAS
  while true #Continue si el tiempo actual es menor al tiempo final de la simulacion
    Numb=probability_function(type_fc;μ=miui,σ=sigmai,α=alphai,θ=thetai); #Hallamos el numero con las caracteristicas inficadas
    aux=+(timenow,Numb,fix) #Hallamos el valor del tiempo que estariamos si lo repararamos
    if aux>t_final
      break;  #Si se salio del tiempo establecido, salir del ciclo
    end
    push!(fail,timenow+Numb)  #Agregamos el momento en que se creo la falla
    timenow=aux;
    push!(fail,timenow)git config --global user.email "EMAIL"
  end
  shift!(fail)  #Eliminamos el 0.0 del vector de fallas ya que solo era para declaracion
  return fail#Retornamos el vector de fallas
end

# FUNCION QUE NOS RETORNA UNA MATRIZ DE EVENTOS TENIENDO EN CUENTA LAS CARACTERISTICAS DE CADA EVENTO
# Recibe como parametros el Numero de iteraciones, el tiempo de simulacion, el tipo de funcion de
# distribucion con sus respectivos parametros despues de ;
function Evento(NumIter,t_final,fix,type_fc,equipo::String;miu=NaN,sigma=NaN,alpha=NaN,theta=NaN)
  global Mat, ID, num
  num+=1
  push!(ID,[num,equipo])
  Aux=[VecAlea(t_final,fix,type_fc;miui=miu,sigmai=sigma,alphai=alpha,thetai=theta)] # Declaramos el vector aleatorio con el primer caso
  for i = 2:NumIter # Realizamos el mismo procedimiento anterior pero Numero de iteraciones - 1
    push!(Aux,VecAlea(t_final,fix,type_fc;miui=miu,sigmai=sigma,alphai=alpha,thetai=theta)) # Agregamos el resultado al vector aleatorio
  end
  push!(Mat,Aux)
  return "Exito: Evento Equipo $equipo Id $num" # Retornamos un vector de vectores de diferentes tamaños
end

# FUNCION QUE NOS PERMITE OPERAR UN PAR DE VECTORES CON UNA FUNCION LOGICA EN CONTINUO
# Recibe como parametros el primer vector "VecA", el segundo "VecB" y la operacion logica "AND" "OR"
function LogicOpVec(VecA,VecB,LogOp)
  eventF=[0.0]  # Declaramos el primer valor de la salida para que posea el formato deseado en la salida
  if LogOp=="OR"  # Seccion de operacion "OR"
    iai=1 # Indice de inicio del Vector A
    iaf=2 # Indice de final del Vector A
    ibi=1 # Indice de inicio del Vector B
    ibf=2 # Indice de final del Vector B
    flagA,flagB=1,1 # Bandera de los vectores A y B que indica el final de los vectores
    findend=1; # Bandera que permite que se agregue el comienzo del vector A o B en la salida
    while true # Ciclo que finalizaa cuando ya se hayan analizado ambos vectoes

      if iaf>endof(VecA[1]) # Si el indice final de A sobrepasa del Vector A regrese
        iaf-=2
        iai-=2
        flagA=0
      end
      if ibf>endof(VecB[1]) # Si el indice final de B sobrepasa del Vector B regrese
        ibf-=2
        ibi-=2
        flagB=0;
      end
      # Si las banderas son 0 y algun indice es mayor al tamaño del vector salga del while
      if (flagA==0 && ibf>endof(VecB[1])) || (flagB==0 && iaf>endof(VecA[1])) || (flagA==0 && flagB==0)
        break;
      end
      # Se agrega al vector de salida el menor de los inicios de los vectores
      if VecA[1][iai]<VecB[1][ibi] && flagA==1
        if findend==1 # Siempre y cuagit config --global user.email "EMAIL"ndo el anterior dato sea un final
          push!(eventF,VecA[1][iai])
        end
        # Se analiza el caso en el que el inicio del vector B este antes del final del vector A
        if VecA[1][iaf]>=VecB[1][ibi] && VecA[1][iaf]<=VecB[1][ibf]
          findend=0;  #  Se activa la bandera indicando que no se ha hallado el final
          iaf+=2  # Pasamos al siguiente caso
          iai+=2
        else
          findend=1 # Significa que podemos agregar el final del evento
          push!(eventF,VecA[1][iaf])  # Agregamos a la salida

          if VecB[1][ibf]<VecA[1][iaf]  # Analizamos el caso que el evento de la vector B este dentro del evento del vector A
            ibf+=2  # Se es asi pasamos al siguente caso de B
            ibi+=2
          end
          # Pasamos al siguente caso de A
          iaf+=2
          iai+=2
        end
        # Miramos el mismo caso del anterior teniendo en cuenta que el menor sera el vector B
      elseif VecA[1][iai]>=VecB[1][ibi] && flagB==1

        if findend==1
          push!(eventF,VecB[1][ibi])
        end

        if VecB[1][ibf]>=VecA[1][iai] && VecB[1][ibf]<=VecA[1][iaf]
          findend=0;
          ibf+=2
          ibi+=2
        else
          findend=1
          push!(eventF,VecB[1][ibf])

          if VecA[1][iaf]<VecB[1][ibf]
            iaf+=2
            iai+=2
          end
          ibf+=2
          ibi+=2
        end
        # Analizamos el caso cuando ya se llego al final del vector A
      elseif VecA[1][iai]<VecB[1][ibi] && flagA==0
        if findend==1 # Si esta disponible agregamos el inicio de B
          push!(eventF,VecB[1][ibi])
        end
        push!(eventF,VecB[1][ibf])  # y el final del vector B
        ibf+=2
        ibi+=2
        # Analizamos el mismo caso anterior pero cuando se llego al final del vector B
      elseif VecA[1][iai]>=VecB[1][ibi] && flagB==0
        if findend==1
          push!(eventF,VecA[1][iai])
        end
        push!(eventF,VecA[1][iaf])
        iaf+=2
        iai+=2
      end
    end
    shift!(eventF)  # Eliminamos el primer valor ya que este fue de declaracion
  end
  return(eventF) # Retornamos el vector de resultado
end

# FUNCION QUE REALIZA LA OPERACION LOGICA DE MATICES
function LogicOpMat(Opera,ID1,ID2) # Recibe La Matriz A, Matriz B y la Operacion
  global Mat,Res,num,ID
  num+=1
  MatA=Mat[ID1]
  MatB=Mat[ID2]

  Numiter=length(MatA)  # Hallamos el numero de iteciones por medio del tamaño de la matriz A
  Res=[LogicOpVec([MatA[1]],[MatB[1]],"OR")] # Declaramos la salida "OUT" haciendo el primer caso
  for i=2:Numiter # Comenzamos desde i=2 porque i=1 se desarrollo en la anterior
    push!(Res,LogicOpVec([MatA[i]],[MatB[i]],"OR")) # Realizamos la operacion por linea
  end

  push!(ID,[num,"Resultado"])
  push!(Mat,Res)
  return "Exito: Operacion Logica $Opera - ID $num"  # Retornamos el valor
end

function DispVect(VecA,t_sim)
  sVecA=length(VecA[1])
  aux=0.0
  for i=1:2:sVecA-2
    aux=+(-(VecA[1][+(i,1)],VecA[1][i]),aux)
  end
  aux=(1-aux/t_sim)*100
  return aux
end

function DispMat(t_sim,Steps)
  global Res
  sRes=length(Res)
  aux=[0.0]
  for i=1:sRes
    push!(aux,DispVect([Res[i]],t_sim))
  end
  shift!(aux)

  plt[:hist](aux,Steps)
  return "Exito: Calculo de disponibilidad"
end

function Tabla()
  global ID,Mat
  return ID,Mat
end



end # module
