Algoritmo PROYECTO_NOMINA
	
	//Emilio Gael Renteria Rayo 263031
	//------------------------------
	// Inicializar variables y asignacion de datos
	//
	total_general <- 0 
	
    Dimensionar dias[7]
    dias[1] <- "Lunes"
    dias[2] <- "Martes"
    dias[3] <- "Miércoles"
    dias[4] <- "Jueves"
    dias[5] <- "Viernes"
    dias[6] <- "Sábado"
    dias[7] <- "Domingo"
    
    Dimensionar tarifa_vuelta[10]
    tarifa_vuelta[1] <- 25
    tarifa_vuelta[2] <- 65
    tarifa_vuelta[3] <- 115
    tarifa_vuelta[4] <- 175
    tarifa_vuelta[5] <- 295
    tarifa_vuelta[6] <- 425
    tarifa_vuelta[7] <- 565
    tarifa_vuelta[8] <- 715
    tarifa_vuelta[9] <- 875
    tarifa_vuelta[10] <- 1045
    //------------------------
	//------------------------
	//
	//Numero de areas a capturar
    Escribir "Ingrese el número de áreas o terminales a capturar:"
    Leer num_areas
    //------------------------
    Para i <- 1 Hasta num_areas Con Paso 1 Hacer
        
        Escribir ' '
        Escribir "  ***** Capturar Área  *****  ", i ,'  ******'
		Escribir ' '
		
        Escribir " - Ingrese el número de empleados de esta área : "
        Leer num_empleados
        
        total_area <- 0
        
        Para j <- 1 Hasta num_empleados Con Paso 1 Hacer
			//-----------------------------
			// Inicializamos variables en 0
            vueltas_totales <- 0
            bono_vueltas <- 0
            bono_asistencia <- 0
            pago_horas_extra <- 0
            bono_festivo <- 0
            descanso_dia <- ""
            //
            //-------------------------
			//
			//------------------------------
			// datos genenrales
            
            Escribir ""
            Escribir "           ***** Datos del Empleado *****"
            Escribir ""
            
            Escribir " -Ingrese el nombre del empleado"
            Leer nombre
            
            Escribir " -Ingrese el número de empleado"
            Leer num_empleado
            
            Escribir " -Ingrese el puesto"
            Leer puesto
            puesto <- Mayusculas(puesto)
            
            Escribir " -Ingrese el sueldo base diario"
            Leer sueldo_diario
            
            Repetir
                Escribir " -Ingrese los días laborados (0 a 6)"
                Leer dias_laborados
            Hasta Que dias_laborados >= 0 Y dias_laborados <= 6
			//--------------------------
            // Aplicacion especial a chofer operador
			
            Si puesto = "CHOFER OPERADOR" o puesto = "CHOFEROPERADOR" Entonces
                //----------
				// condicional para saber dias de descanso
                Escribir ""
                Escribir "cual es su día de descanso ?"
                Para seleccion_dia <- 1 Hasta 7 Con Paso 1 Hacer
                    Escribir seleccion_dia, ". ", dias[seleccion_dia]
                FinPara
                
                Repetir
                    Escribir "Ingrese el número (1-7):"
                    Leer seleccion_dia
                Hasta Que seleccion_dia >= 1 Y seleccion_dia <= 7
                
                descanso_dia <- dias[seleccion_dia]
                //-------
				//
				//-------------------
				// Bonificador por vuelta
                Escribir ""
                Escribir "Ingrese las vueltas realizadas por día:"
                Para dia_actual <- 1 Hasta 7 Con Paso 1 Hacer
                    Si dias[dia_actual] <> descanso_dia Entonces
                        
                        Escribir "Vueltas del día ", dias[dia_actual], " (0 a 10):"
                        Leer vueltas_dia
                        Bono_dia_vuelta <- tarifa_vuelta[vueltas_dia] 
						acum<-acum+Bono_dia_vuelta
                        vueltas_totales <- vueltas_totales + vueltas_dia
                    FinSi
                FinPara
				//-------------------------
            FinSi
            //------------------------
			// preguntas generales
            Escribir " -Trabajó en su día de descanso? (SI o NO):"
            Leer trabajo_descanso
            trabajo_descanso <- Mayusculas(trabajo_descanso)
            
            Escribir " -Trabajó en día festivo? (SI o NO):"
            Leer trabajo_festivo
            trabajo_festivo <- Mayusculas(trabajo_festivo)
            //-------------------------
            // HORAS EXTRA (Chofer NO)
            Si puesto = "CHOFER OPERADOR" o puesto = "CHOFEROPERADOR"  Entonces
                pago_horas_extra <- 0
            Sino
                Escribir "¿Trabajó horas extra? (SI o NO):"
                Leer trabajo_extra
                trabajo_extra <- Mayusculas(trabajo_extra)
                
                Si trabajo_extra = "SI" Entonces
                    
                    Escribir "Ingrese horas extra totales:"
                    Leer horas_extra
                    
                    Si horas_extra <= 8 Entonces
                        pago_horas_extra <- horas_extra * (sueldo_diario / 8) * 2
                    Sino
                        horas_dobles <- 8
                        horas_triples <- horas_extra - 8
                        pago_horas_extra <- (horas_dobles * (sueldo_diario / 8) * 2) + (horas_triples * (sueldo_diario / 8) * 3)
                    FinSi
                FinSi
            FinSi
            //-------
			// BOno por asistencia
            Escribir "Ingrese el número de faltas:"
            Leer faltas
			//--------------------
            // Calculo de sueldo base
			
            sueldo_semana <- sueldo_diario * dias_laborados
			//-----------------
            
            // Chofer cobra 7 si trabaja 6 días
            Si puesto = "CHOFER OPERADOR" o puesto = "CHOFEROPERADOR" Entonces
                Si dias_laborados = 6 Entonces
                    sueldo_semana <- sueldo_diario * 6
                FinSi
            FinSi
            //---------------
            // Trabajo en descanso
            Si trabajo_descanso = "SI" Entonces
                sueldo_semana <- sueldo_semana + sueldo_diario
            FinSi
            //--------------------
            // Festivo triple (2 días extra)
            Si trabajo_festivo = "SI" Entonces
                bono_festivo <- sueldo_diario * 2
            FinSi
            //----------------------
            // Bono del Chofer
            Si puesto = "CHOFER OPERADOR" Entonces
                
                Si vueltas_totales >= 1 Y vueltas_totales <= 10 Entonces//------------------------------------------------------------
                    bono_vueltas <- tarifa_vuelta[vueltas_totales]
                FinSi
                
                Si faltas = 0 Entonces
                    bono_asistencia <- 125
                FinSi
				
			
            FinSi
            
            total_bono <- acum + bono_asistencia + bono_festivo + pago_horas_extra 
            
            //------------------------------------------
			//-------------------
            // Calculo de deducciones
			infonavit <- (sueldo_semana + total_bono) * 0.05
			imss <- (sueldo_semana +total_bono)* 0.02
            base_isr <- sueldo_semana + total_bono
			//
			// Calculo del ISR
            Si base_isr >= 509.47 Y base_isr <= 1027.52 Entonces
                isr <- 53.95
            Sino
                Si base_isr >= 1027.53 Y base_isr <= 1619.51 Entonces
                    isr <- 164.61
                Sino
                    Si base_isr >= 1619.52 Y base_isr <= 3091.90 Entonces
                        isr <- 303.61
                    Sino
                        Si base_isr >= 3091.91 Entonces
                            isr <- 745.56
                        Sino
                            isr <- 0
                        FinSi
                    FinSi
                FinSi
            FinSi
			
            //---------------------------------
            Escribir "¿Desea agregar un descuento adicional? (SI o NO):"
            Leer agregar_descuento
            agregar_descuento <- Mayusculas(agregar_descuento)
            //--------------
			// Inicializando Variable
            monto_descuento <- 0
			//-------------
            Si agregar_descuento = "SI" Entonces
                Escribir " Ingresa el Porque? descuento:"
                Leer concepto_descuento
                Escribir " Ingresa el monto:"
                Leer monto_descuento
            FinSi
            //-----------
			// Calculo de deducciones y sumatorias
            total_deducciones <- imss + infonavit + isr + monto_descuento
            total_pagar <- sueldo_semana + total_bono + pago_horas_extra - total_deducciones 
            total_area <- total_area + total_pagar
            total_general <- total_general + total_pagar
            
            //
            //--------------------------------------------------
			// Datos del trabajador
            
            Escribir "Área: ", i
            Escribir "Empleado: ", nombre
            Escribir "Número: ", num_empleado
            Escribir "Puesto: ", puesto
            
            Si puesto = "CHOFER OPERADOR" o puesto = "CHOFEROPERADOR"   Entonces
                Escribir "Día de descanso: ", descanso_dia
                Escribir "Vueltas totales: ", vueltas_totales
                Escribir "Bono por vueltas: $", acum
                Escribir "Bono asistencia: $", bono_asistencia
			SiNo
				Escribir "Día de descanso: ", descanso_dia
            FinSi
            
            Si trabajo_festivo = "SI" Entonces
                Escribir "Bono día festivo (triple): $", bono_festivo
            FinSi
            
            Si pago_horas_extra > 0 Entonces
                Escribir "Horas extra pagadas: $", pago_horas_extra
            FinSi
            
            Escribir "Sueldo semanal base: $", sueldo_semana
            Escribir "IMSS: $", imss
            Escribir "ISR: $", isr
            Escribir "INFONAVIT: $", infonavit
            Escribir "Descuento adicional: $", monto_descuento
            Escribir "Total deducciones: $", total_deducciones
            Escribir "TOTAL A PAGAR: $", total_pagar
            Escribir " |***********************************************| "
            Escribir ""
            
        FinPara
        
        Escribir ""
        Escribir "      ***** Total del area *****", i, ": $", total_area
        Escribir ""
        
    FinPara
    
    Escribir ""
    Escribir "    ***** Saldo total de nomina: $", total_general
    Escribir ""

FinAlgoritmo

