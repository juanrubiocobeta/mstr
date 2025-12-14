* Encoding: UTF-8.
* ***************************************************************.
* EJERCICIO 1: MODELO IDENTIDAD (CON 10 OBJETOS).
* Lista de variables explícita.
* ***************************************************************.
PROXSCAL
  /VARIABLES= P1_2 P1_3 P2_3 P1_4 P2_4 P3_4 P1_5 P2_5 P3_5 P4_5 P1_6 P2_6 P3_6 P4_6 P5_6 P1_7 P2_7 P3_7 P4_7 P5_7 P6_7 P1_8 P2_8 P3_8 P4_8 P5_8 P6_8 P7_8 P1_9 P2_9 P3_9 P4_9 P5_9 P6_9 P7_9 P8_9 P1_10 P2_10 P3_10 P4_10 P5_10 P6_10 P7_10 P8_10 P9_10
  /SOURCES=Sujeto
  /MODEL=IDENTITY
  /OPTIONS=OBJECTS(10).

* ***************************************************************.
* EJERCICIO 2: MODELO INDSCAL (CON 10 OBJETOS).
* Lista de variables explícita.
* ***************************************************************.
PROXSCAL
  /VARIABLES= P1_2 P1_3 P2_3 P1_4 P2_4 P3_4 P1_5 P2_5 P3_5 P4_5 P1_6 P2_6 P3_6 P4_6 P5_6 P1_7 P2_7 P3_7 P4_7 P5_7 P6_7 P1_8 P2_8 P3_8 P4_8 P5_8 P6_8 P7_8 P1_9 P2_9 P3_9 P4_9 P5_9 P6_9 P7_9 P8_9 P1_10 P2_10 P3_10 P4_10 P5_10 P6_10 P7_10 P8_10 P9_10
  /SOURCES=Sujeto
  /MODEL=INDSCAL
  /OPTIONS=OBJECTS(10).
