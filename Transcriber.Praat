#############################################
#############################################
#########Transcriptor fonológico - Español de Chile
#############################################
#############################################
#####License: 
###This script is under the terms of the GNU General Public License version 3 or any later version. 
###This means this script is free software and you can use, change, and share it
###For further details, you can find the GNU General Public License at https://www.gnu.org/
#####Made by Dario Fuentes Grandon, dario.fuentes.grandon@gmail.com
#####Description:
###Este script realiza una transcripción a símbolos fonológicos partir de un tier con una transcripción ortográfica. 
###Se generará un nuevo archivo TextGrid que incluirá un nuevo tier con la transcripción fonológica.
###Las reglas de la transcripción se basan en el español de Chile. Esta transcripción NO cubre todos los casos posibles, por lo que requiere de revisión.
###No incluí archifonemas ni semoconsonantes. De todos modos, las reglas de transcripción son fáciles de modificar, ver sección *****Reglas de Transcripción*****.
###Y recuerden, el fonema no existe.
#############################################

form Rutas de archivos y parámetros
	comment Directorio de TextGrids
	text dir_TextGrid c:\temp\
	comment Directorio de TextGrids nuevos
	text new_dir_TextGrid c:\temp\
	sentence textGrid_file_extension .TextGrid
	comment Tier a transcribir
	real t_Tier 1
	comment Posición del nuevo tier
	real new_Tier 2
	comment Nombre del nuevo tier
	text new_Tier_name "Phon"
endform

#Lista de archivos del directorio de origen
id_string_TextGrid = Create Strings as file list: "list", dir_TextGrid$+ "/*" +textGrid_file_extension$
n_files = Get number of strings
writeInfoLine: "I am the code of my words"

#Loop de archivos
for x to n_files
	#Apertura de archivos
	selectObject: id_string_TextGrid
	filename_TextGrid$ = Get string: x
	id_TextGrid = Read from file: dir_TextGrid$ + filename_TextGrid$

	#Nuevo tier: copia del tier de referencia con intervalos vacíos
	Duplicate tier: t_Tier, new_Tier, new_Tier_name$
	Replace interval texts: new_Tier, 1, 0, ".*", "", "Regular Expressions"

	#Número de intervalos del tier de referencia
	n_Int = Get number of intervals: t_Tier

	#Loop de intervalos
	for int_x to n_Int
	  #Etiqueta de intervalo
	  lab_int$ = Get label of interval: t_Tier, int_x
	  #Loop de reduccion a minusculas para facilitar la transcripción
	  for i to 36
    	mayus$ = mid$ ("ABCDEFGHIJKLMNOPQRSTUVWXYZáéíóúÁÉÍÓÚ", i, 1)
    	minus$ = mid$ ("abcdefghijklmnopqrstuvwxyzaeiouaeiou", i, 1)
	    #regex
    	lab_int$ = replace_regex$ (lab_int$, mayus$, minus$, 0)
	  endfor
	  #Largo de la etiqueta
	  lab_length = length(lab_int$)
	  #etiqueta fonologica vacia
	  phon_lab$ = ""
	  #Loop de transcripción
	  for letter_x to lab_length
	    #Extraer letra a transcribir, letra anterior y letra posterior
		if letter_x = lab_length
		  symbol$ = mid$(lab_int$, letter_x, 1)
	      pre_symbol$ = mid$(lab_int$, letter_x-1, 1)
	      post_symbol$ = ""
	    elsif letter_x > 1
	      symbol$ = mid$(lab_int$, letter_x, 1)
	      pre_symbol$ = mid$(lab_int$, letter_x-1, 1)
	      post_symbol$ = mid$(lab_int$, letter_x+1, 1)
	    elsif letter_x = 1
	      symbol$ = mid$(lab_int$, letter_x, 1)
	      pre_symbol$ = ""
	      post_symbol$ = mid$ (lab_int$, letter_x+1, 1)
	    endif
	    ##################################
	    #*****Reglas de Transcripción*****
	    ##################################
	    #b y v
	    if symbol$ = "b" or symbol$ = "v"
	      n_symbol$ = "b"
	    #c
	    elsif symbol$ = "c"
	      if post_symbol$ = "h"
	        n_symbol$ = "ʧ"
	      elsif post_symbol$ = "e" or post_symbol$ = "i" or post_symbol$ = "y"
	      	n_symbol$ = "s"
	      else
	      	n_symbol$ = "k"
	      endif
	    #d
	    elsif symbol$ = "d"
	      n_symbol$ = "d"
	    #f
	    elsif symbol$ = "f"
	      n_symbol$ = "f"
	    #g
	    elsif symbol$ = "g"
	      if post_symbol$ = "e" or post_symbol$ = "i"
	        n_symbol$ = "x"
	      else
	      	n_symbol$ = "g"
	      endif
	    #h
	    elsif symbol$ = "h"
	      n_symbol$ = ""
	    #j
	    elsif symbol$ = "j"
	      n_symbol$ = "x"
	    #k
	    elsif symbol$ = "k"
	      n_symbol$ = "k"
	    #l
	    elsif symbol$ = "l"
	      if post_symbol$ = "l"
	        n_symbol$ = "ʤ"
	      elsif pre_symbol$ = "l"
	        n_symbol$ = ""
	      else 
	      	n_symbol$ = "l"
	      endif
	    #m
	    elsif symbol$ = "m"
	      n_symbol$ = "m"
	    #n
	    elsif symbol$ = "n"
	      n_symbol$ = "n"
	    #ñ
	    elsif symbol$ = "ñ"
	      n_symbol$ = "ɲ"
	    #p
	    elsif symbol$ = "p"
	      n_symbol$ = "p"
	    #q
	    elsif symbol$ = "q"
	      n_symbol$ = "k"
	    #r
	    elsif symbol$ = "r"
	      if pre_symbol$ = " " or pre_symbol$ = "" or pre_symbol$ = "r" or post_symbol$ = "r"
	        n_symbol$ = "r"
	      else
	      	n_symbol$ = "ɾ"
	      endif
	    #s
	    elsif symbol$ = "s"
	      n_symbol$ = "s"
	    #t
	    elsif symbol$ = "t"
	      n_symbol$ = "t"
	    #w
	    elsif symbol$ = "w"
	      n_symbol$ = "u"
	    #x
	    elsif symbol$ = "x"
	      n_symbol$ = "ks"
	    #y
	    elsif symbol$ = "y"
	      if post_symbol$ = "a" or post_symbol$ = "e" or post_symbol$ = "i" or post_symbol$ = "o" or post_symbol$ = "u"
	        n_symbol$ = "ʤ"
	      else
	      	n_symbol$ = "i"
	      endif
	    #z
	    elsif symbol$ = "z"
	      n_symbol$ = "s"
	    #u
	    elsif symbol$ = "u"
	      if pre_symbol$ = "q"
	        n_symbol$ = ""
	      elsif pre_symbol$ = "g" and post_symbol$ = "e" or pre_symbol$ = "g" and post_symbol$ = "i" 
	        n_symbol$ = ""
	      else
	      	n_symbol$ = "u"
	      endif
	    #remover puntuación
	    elsif symbol$ = "." or symbol$ = "," or symbol$ = ";"
	      n_symbol$ = ""
	    #otras vocales y pausas
	    else
	      n_symbol$ = symbol$
	    endif
	    ##################################
	    #*****Fin de Jumps de reglas*****
	    ##################################
	    #actualización de la etiqueta fonlógica
	    phon_lab$ = phon_lab$ + n_symbol$
	  endfor
	  #Inserción de la etiqueta fonológica al intervalo del tier fonológico
	  Set interval text: new_Tier, int_x, phon_lab$
	endfor
	#Guardar nuevo TextGrid
	Save as text file: new_dir_TextGrid$ + "New_" + filename_TextGrid$

	#Limpieza
	selectObject: id_TextGrid
	Remove
endfor 
selectObject: id_string_TextGrid
Remove





