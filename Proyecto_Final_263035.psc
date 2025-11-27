Algoritmo SistemaNominaCompleta
	// Emilio Gael Renteria Rayo 263035
	
	//
	Dimensionar dias[7]
	dias[1] <- "LUNES"
	dias[2] <- "MARTES"
	dias[3] <- "MIÉRCOLES"
	dias[4] <- "JUEVES"
	dias[5] <- "VIERNES"
	dias[6] <- "SÁBADO"
	dias[7] <- "DOMINGO"
	
	//
	Dimensionar orden_vueltas[7]
	orden_vueltas[1] <- 4 
	orden_vueltas[2] <- 5 
	orden_vueltas[3] <- 6 
	orden_vueltas[4] <- 7 
	orden_vueltas[5] <- 1 
	orden_vueltas[6] <- 2 
	orden_vueltas[7] <- 3 
	
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
	
	
	Escribir "Ingrese el número de áreas o terminales a capturar:"
	Leer num_areas
	total_general <- 0
	// BUCLE PRINCIPAL: Áreas
	Para i_area <- 1 Hasta num_areas Con Paso 1 Hacer
		Escribir "  *************************************************  "
		Escribir "  *         CAPTURA PARA EL ÁREA ", i_area,"                *  "
		Escribir "  *************************************************  "
		
		Escribir "Ingrese el número de empleados de esta área:"
		Leer num_empleados
		
		total_area <- 0
		
		// BUCLE SECUNDARIO: Empleados
		Para j_empleado <- 1 Hasta num_empleados Con Paso 1 Hacer
			
			// Inicialización de acumuladores por empleado
			vueltas_totales <- 0
			acum_bono_vueltas <- 0
			bono_asistencia <- 0
			pago_horas_extra <- 0
			bono_festivo_extra <- 0 
			descanso_dia <- ""
			vueltas_descanso <- 0
			
			Escribir ""
			Escribir "----------------------------------------------"
			Escribir "           REGISTRO DEL TRABAJADOR ", j_empleado
			Escribir "----------------------------------------------"
			
			Escribir "Ingrese el nombre del empleado:"
			Leer nombre
			
			// Eliminación de espacios al final del nombre
			Si Longitud(nombre) > 0 Entonces
				Mientras Subcadena(nombre, Longitud(nombre), Longitud(nombre)) = " " Y Longitud(nombre) > 0 Hacer
					nombre <- Subcadena(nombre, 1, Longitud(nombre) - 1)
				FinMientras
			FinSi
			
			Escribir "Ingrese el número de empleado:"
			Leer num_empleado
			
			Escribir "Ingrese el puesto:"
			Leer puesto
			
			// Eliminación de espacios al final del puesto
			Si Longitud(puesto) > 0 Entonces
				Mientras Subcadena(puesto, Longitud(puesto), Longitud(puesto)) = " " Y Longitud(puesto) > 0 Hacer
					puesto <- Subcadena(puesto, 1, Longitud(puesto) - 1)
				FinMientras
			FinSi
			
			puesto <- Mayusculas(puesto)
			
			Escribir "Ingrese el sueldo base diario:"
			Leer sueldo_diario
			
			Repetir
				Escribir "Ingrese los días laborados (0 a 6):"
				Leer dias_laborados
			Hasta Que dias_laborados >= 0 Y dias_laborados <= 6
			
			// CAPTURA DE INFORMACIÓN ESPECÍFICA PARA CHOFER OPERADOR
			Si puesto = "CHOFER OPERADOR" Entonces
				
				// 1. Preguntar por el día de descanso
				Escribir ""
				Escribir "Seleccione el día de descanso (Para el cálculo de vueltas):"
				Para seleccion_dia <- 1 Hasta 7 Con Paso 1 Hacer
					Escribir seleccion_dia, ". ", dias[seleccion_dia]
				FinPara
				
				Repetir
					Escribir "Ingrese el número (1-7):"
					Leer seleccion_dia
				Hasta Que seleccion_dia >= 1 Y seleccion_dia <= 7
				
				descanso_dia <- dias[seleccion_dia]
				
				// 2. Captura de vueltas por día
				Escribir ""
				Escribir "Ingrese las vueltas realizadas por día (Jueves a Miércoles):"
				Para i_orden_vuelta <- 1 Hasta 7 Con Paso 1 Hacer
					dia_actual_indice <- orden_vueltas[i_orden_vuelta]
					dia_nombre <- dias[dia_actual_indice]
					
					Si dia_nombre <> descanso_dia Entonces
						
						Escribir "Vueltas del día ", dia_nombre, " (0 a 10):"
						Leer vueltas_dia
						
						// Asegurar que las vueltas estén en el rango de 0 a 10
						Si vueltas_dia < 0 O vueltas_dia > 10 Entonces
							vueltas_dia <- 0
						FinSi
						
						// Calcular bono individual y acumulamos
						Si vueltas_dia > 0 Entonces
							Bono_dia_vuelta <- tarifa_vuelta[vueltas_dia]
							acum_bono_vueltas <- acum_bono_vueltas + Bono_dia_vuelta
						FinSi
						
						vueltas_totales <- vueltas_totales + vueltas_dia
					FinSi
				FinPara
				
			FinSi
			
			Escribir ""
			Escribir "¿Trabajó en su día de descanso (", descanso_dia, ")? (S/N):"
			Leer trabajo_descanso
			trabajo_descanso <- Mayusculas(trabajo_descanso)
			
			// 3. Preguntar por vueltas en día de descanso si aplica
			Si puesto = "CHOFER OPERADOR" Entonces
				Si trabajo_descanso = "S" Entonces
					Escribir "Ingrese las vueltas realizadas en su día de descanso (", descanso_dia, ") (0 a 10):"
					Leer vueltas_descanso
					
					// Aseguramos que las vueltas estén en el rango de 0 a 10
					Si vueltas_descanso < 0 O vueltas_descanso > 10 Entonces
						vueltas_descanso <- 0
					FinSi
					
					Si vueltas_descanso > 0 Entonces
						Bono_descanso_vuelta <- tarifa_vuelta[vueltas_descanso]
						acum_bono_vueltas <- acum_bono_vueltas + Bono_descanso_vuelta
					FinSi
					
					vueltas_totales <- vueltas_totales + vueltas_descanso
				FinSi
			FinSi
			
			Escribir "¿Trabajó en día festivo? (S/N):"
			Leer trabajo_festivo
			trabajo_festivo <- Mayusculas(trabajo_festivo)
			
			// HORAS EXTRA (para el chofer no)
			Si puesto = "CHOFER OPERADOR" Entonces
				pago_horas_extra <- 0
			Sino
				Escribir "¿Trabajó horas extra? (S/N):"
				Leer trabajo_extra
				trabajo_extra <- Mayusculas(trabajo_extra)
				
				Si trabajo_extra = "S" Entonces
					
					Escribir "Ingrese horas extra totales:"
					Leer horas_extra
					
					// HORAS EXTRA
					Si horas_extra <= 9 Entonces  
						pago_horas_extra <- horas_extra * (sueldo_diario / 8) * 2 
					Sino
						horas_dobles <- 9 
						horas_triples <- horas_extra - 9 
						pago_horas_extra <- (horas_dobles * (sueldo_diario / 8) * 2) + (horas_triples * (sueldo_diario / 8) * 3) 
					FinSi
				FinSi
			FinSi
			
			Escribir "Ingrese el número de faltas:"
			Leer faltas
			
			// CÁLCULOS DE PAGO 
			// 1. Sueldo Base Bruto
			sueldo_base_bruto_7dias <- sueldo_diario * 7
			// 2. DEDUCCIÓN POR FALTAS
			deduccion_faltas <- 0
			Si faltas > 0 Entonces
				// Descontar el valor de los días faltados de la base semanal bruta.
				deduccion_faltas <- sueldo_diario * faltas 
			FinSi
			
			// Sueldo Base Neto 
			sueldo_semana <- sueldo_base_bruto_7dias - deduccion_faltas 
			
			// 3. PAGO EXTRA POR DÍA DE DESCANSO TRABAJADO 
			pago_descanso_extra <- 0
			Si trabajo_descanso = "S" Entonces
				pago_descanso_extra <- sueldo_diario 
			FinSi
			
			// 4. PAGO EXTRA POR DÍA FESTIVO TRABAJADO 
			Si trabajo_festivo = "S" Entonces
				bono_festivo_extra <- sueldo_diario * 2 
			FinSi
			
			// BONOS 
			bono_asistencia <- 0
			Si puesto = "CHOFER OPERADOR" Entonces
				Si faltas = 0 Entonces
					bono_asistencia <- 125
				FinSi
			FinSi
			total_bono <- acum_bono_vueltas + bono_asistencia + bono_festivo_extra + pago_descanso_extra
			percepciones_brutas <- sueldo_semana + total_bono + pago_horas_extra 
			
			// DEDUCCIONES 
			infonavit_base <- percepciones_brutas
			imss_base <- percepciones_brutas
			
			infonavit <- infonavit_base * 0.05 
			imss <- imss_base * 0.02 
			
			base_isr <- percepciones_brutas - (imss + infonavit) 
			
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
			
			Escribir "¿Desea agregar un descuento adicional? (S/N):"
			Leer agregar_descuento
			agregar_descuento <- Mayusculas(agregar_descuento)
			
			monto_descuento <- 0
			concepto_descuento <- "" 
			Si agregar_descuento = "S" Entonces
				Escribir "Ingrese el concepto del descuento:"
				Leer concepto_descuento
				Escribir "Ingrese el monto:"
				Leer monto_descuento
			FinSi
			
			total_deducciones <- imss + infonavit + isr + monto_descuento 
			
			total_pagar <- percepciones_brutas - total_deducciones 
			
			total_area <- total_area + total_pagar
			total_general <- total_general + total_pagar
			
			Escribir ""
			Escribir "----------------------------------------------"
			Escribir "           RESUMEN DEL TRABAJADOR"
			Escribir "----------------------------------------------"
			Escribir "Área: ", i_area
			Escribir "Empleado: ", nombre
			Escribir "Número: ", num_empleado
			Escribir "Puesto: ", puesto
			
			Si puesto = "CHOFER OPERADOR" Entonces
				Escribir "Día de descanso: ", descanso_dia
				Escribir "Trabajó en descanso: ", trabajo_descanso
				Si trabajo_descanso = "S" Entonces
					Escribir "Vueltas en descanso: ", vueltas_descanso
				FinSi
				Escribir "Vueltas totales: ", vueltas_totales
				Escribir "Bono total por vueltas: $", acum_bono_vueltas
				Escribir "Bono asistencia: $", bono_asistencia
			FinSi
			
			Si trabajo_festivo = "S" Entonces
				Escribir "Bono día festivo (triple): $", bono_festivo_extra 
			FinSi
			
			Si pago_horas_extra > 0 Entonces
				Escribir "Horas extra pagadas: $", pago_horas_extra
			FinSi
			
			Escribir "Sueldo semanal base: $", sueldo_semana 
			Escribir "IMSS: $", imss
			Escribir "ISR: $", isr
			Escribir "INFONAVIT: $", infonavit
			Escribir "Descuento adicional: $", monto_descuento
			Si agregar_descuento = "S" Entonces
				Escribir "Concepto de descuento: ", concepto_descuento
			FinSi
			Escribir "Total deducciones: $", total_deducciones
			Escribir "TOTAL A PAGAR: $", total_pagar
			Escribir "----------------------------------------------"
			Escribir ""
			
		FinPara
		
		Escribir "----------------------------------------------"
		Escribir "           TOTAL DEL ÁREA ", i_area, ": $", total_area
		Escribir "----------------------------------------------"
		Escribir ""
		
	FinPara
	
	Escribir "=============================================="
	Escribir "           TOTAL GENERAL DE LA NÓMINA: $", total_general
	Escribir "=============================================="
	
FinAlgoritmo