       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 CBLPGSHT.
       AUTHOR.                     PHILIP GINGERICH.
       DATE-WRITTEN.               1/19/2017.
       DATE-COMPILED.   
       
      *****************************************************************
      * 
      *    THIS PROGRAM WILL EVALUATE ORDERS IN THE ORDER FILE TO
      *    DETERMINE ANY INVENTORY SHORTAGES. IT WILL OUTPUT TWO    
      *    FLAT FILES AND TWO REPORTS, WHICH REPRESENT THE OVERALL
      *    SHORTAGE BY PART, AND THE PART SHORTAGE BY MACHINE.
      * 
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT MACHINE-MASTER
               ASSIGN TO "MASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT INVENTORY-DATA
               ASSIGN TO "INVENTORY.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT ORDER-DATA
               ASSIGN TO "ORDER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT PRTOUT1
               ASSIGN TO "PARTSHT.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.
               
           SELECT PRTOUT2
               ASSIGN TO "MACHSHT.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.
               
           SELECT FLATOUT1
               ASSIGN TO "PARTSHT.DAT"
               ORGANIZATION IS RECORD SEQUENTIAL.
               
           SELECT FLATOUT2
               ASSIGN TO "MACHSHT.DAT"
               ORGANIZATION IS RECORD SEQUENTIAL.
               
           SELECT FLATOUT3
               ASSIGN TO "MACHSHTCOST.DAT"
               ORGANIZATION IS RECORD SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  MACHINE-MASTER
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 13 CHARACTERS
           DATA RECORD IS MACHINE-RECORD.
           
       01  MACHINE-RECORD.
           05  I-MACHINE           PIC X(5).
           05  I-MACHINE-PART      PIC X(5).
           05  I-PART-QTY          PIC 999.
           
       FD  INVENTORY-DATA
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 16 CHARACTERS
           DATA RECORD IS INVENTORY-RECORD.
           
       01  INVENTORY-RECORD.
           05  I-INV-PART          PIC X(5).
           05  I-INV-QTY           PIC 9(5).
           05  I-INV-COST          PIC 9(4)V99.
           
       FD  ORDER-DATA
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 8 CHARACTERS
           DATA RECORD IS ORDER-RECORD.
           
       01  ORDER-RECORD.
           05  I-ORD-MACHINE       PIC X(5).
           05  I-ORD-NUM           PIC 9(3).
       
       FD  PRTOUT1
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 80 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 55
           DATA RECORD IS RPT1-LN.
           
       01  RPT1-LN                 PIC X(80).
       
       FD  PRTOUT2
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 80 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 55
           DATA RECORD IS RPT2-LN.
           
       01  RPT2-LN                 PIC X(80).
       
       FD  FLATOUT1
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 10 CHARACTERS
           DATA RECORD IS PARTSHT-LN.
           
       01  PARTSHT-LN.
           05  O-TOT-PART          PIC X(5).
           05  O-TOT-SHT           PIC 9(5).
           
       FD  FLATOUT2
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 15 CHARACTERS
           DATA RECORD IS MACHSHT-LN.
           
       01  MACHSHT-LN.
           05  O-MACH              PIC X(5).
           05  O-MACH-PART         PIC X(5).
           05  O-MACH-SHT          PIC 9(5).
           
       FD  FLATOUT3
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 15 CHARACTERS
           DATA RECORD IS MACHSHT-COST-LN.
           
       01  MACHSHT-COST-LN.
           05  O-CST-MACH          PIC X(5).
           05  O-CST-COST          PIC 9(8)V99.
       
       WORKING-STORAGE SECTION.
       
       01  WORK-AREA.
           05  WK-HOLD             PIC X.
           05  C-PCTR1             PIC 99          VALUE 0.
           05  C-PCTR2             PIC 99          VALUE 0.
           05  MORE-RECS           PIC X           VALUE 'Y'.
               88  NO-MORE-RECS    VALUE 'N'.
           05  SUB                 PIC 9(3)        VALUE 1.
           05  SUB-M               PIC 9(3)        VALUE 1.
           05  SUB-I               PIC 9(3)        VALUE 1.
           05  SUB-O               PIC 9(3)        VALUE 1.
           05  MAX-TBL-SIZE        PIC 9(3)        VALUE 100.
               
       01  MACHINE-FILE-DATA.
           05  MACHINE-TABLE OCCURS 100 TIMES INDEXED BY IDX-MACH.
               10  TBL-M-MACHINE   PIC X(5).
               10  TBL-M-PART      PIC X(5).
               10  TBL-M-QTY       PIC 9(3).
               10  TBL-M-SHORT     PIC S9(5).
               
       01  INVENTORY-FILE-DATA.
           05  INVENTORY-TABLE OCCURS 100 TIMES INDEXED BY IDX-INV.
               10  TBL-I-PART      PIC X(5).
               10  TBL-I-QTY       PIC 9(5).
               10  TBL-I-SHORT     PIC S9(5).
               
       01  ORDER-FILE-DATA.
           05  ORDER-TABLE OCCURS 100 TIMES INDEXED BY IDX-ORD.
               10  TBL-O-MACHINE   PIC X(5).
               10  TBL-O-QTY       PIC 9(3).
           
       01  CURRENT-DATE.
           05  I-SYS-DATE.
               10  SYS-YEAR    PIC 9(4).
               10  SYS-MONTH   PIC 99.
               10  SYS-DAY     PIC 99.
           
       01  CO-TITLE-LINE.
           05  FILLER              PIC X(6)    VALUE "DATE: ".
           05  O-TODAY             PIC X(10).
           05  FILLER              PIC X(11)   VALUE SPACES.
           05  FILLER              PIC X(45)   VALUE 
                                           "GINGERICH'S MANUFACTURING".
           05  FILLER              PIC X(6)    VALUE 'PAGE:'.
           05  O-PCTR              PIC Z9.
           
       01  RPT1-TITLE-LINE.
           05  FILLER              PIC X(27)   VALUE SPACES.
           05  FILLER              PIC X(26)   VALUE 
                                   'PART SHORTAGE TOTAL REPORT'.
                                   
       01  RPT2-TITLE-LINE.
           05  FILLER              PIC X(26)   VALUE SPACES.
           05  FILLER              PIC X(28)   VALUE
                                   'MACHINE PART SHORTAGE REPORT'.
           
       01  RPT1-COL-HDG-LINE.
           05  FILLER              PIC X(23)   VALUE SPACES.
           05  FILLER              PIC X(29)   VALUE 'PART'.
           05  FILLER              PIC X(5)    VALUE 'SHORT'.
           
       01  RPT2-COL-HDG-LINE.
           05  FILLER              PIC X(16)   VALUE SPACES.
           05  FILLER              PIC X(21)   VALUE 'MACHINE'.
           05  FILLER              PIC X(21)   VALUE 'PART'.
           05  FILLER              PIC X(5)    VALUE 'SHORT'.
           
       01  D1.
           05  FILLER              PIC X(23)   VALUE SPACES.
           05  O-PART              PIC X(5).
           05  FILLER              PIC X(23)   VALUE SPACES.
           05  O-SHORT             PIC ZZ,ZZZ.
           
       01  D2.
           05  FILLER              PIC X(16)   VALUE SPACES.
           05  O-MACHINE           PIC X(5).
           05  FILLER              PIC X(16)   VALUE SPACES.
           05  O-MACHINE-PART      PIC X(5).
           05  FILLER              PIC X(16)   VALUE SPACES.
           05  O-MACH-PART-SHORT   PIC ZZ,ZZ9.
           
       01 BLANK-LINE               PIC X       VALUE SPACE.
           
       PROCEDURE DIVISION.
       
       L1-CBLPGSHT.
           PERFORM L2-INIT THRU L2-INIT-EXIT.
           PERFORM L2-MAINLINE THRU L2-MAINLINE-EXIT
               WITH TEST BEFORE UNTIL  TBL-O-MACHINE(SUB-O) = SPACES
                                OR     SUB-O > MAX-TBL-SIZE.
           PERFORM L2-CLOSINGS THRU L2-CLOSINGS-EXIT.
           STOP RUN.
           
       L2-INIT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE.
           STRING  SYS-MONTH   DELIMITED BY SIZE
                   '/'         DELIMITED BY SIZE
                   SYS-DAY     DELIMITED BY SIZE
                   '/'         DELIMITED BY SIZE
                   SYS-YEAR    DELIMITED BY SIZE
           INTO O-TODAY.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > MAX-TBL-SIZE
               INITIALIZE MACHINE-TABLE(SUB) INVENTORY-TABLE(SUB)
                          ORDER-TABLE(SUB).
           MOVE 1 TO SUB SUB-M SUB-I SUB-O.
           PERFORM L3-OPENS THRU L3-OPENS-EXIT.
           PERFORM L3-READS THRU L3-READS-EXIT.
           PERFORM L3-CLOSE-INPUTS THRU L3-CLOSE-INPUTS-EXIT.
           PERFORM L9-RPT1-HDGS THRU L9-RPT1-HDGS-EXIT.
           PERFORM L9-RPT2-HDGS THRU L9-RPT2-HDGS-EXIT.
       L2-INIT-EXIT.
           EXIT.
       
       L2-MAINLINE.
           SET IDX-MACH TO 1.
           SEARCH MACHINE-TABLE
               AT END DISPLAY 'ERROR - MACHINE RECORD NOT FOUND'
               WHEN TBL-M-MACHINE(IDX-MACH) = TBL-O-MACHINE(SUB-O)
                   SET SUB-M TO IDX-MACH
                   PERFORM L3-MACH-LOOP THRU L3-MACH-LOOP-EXIT
                       UNTIL TBL-M-MACHINE(SUB-M)
                           NOT EQUAL TBL-O-MACHINE(SUB-O).
           ADD 1 TO SUB-O.
       L2-MAINLINE-EXIT.
           EXIT.
       
       L2-CLOSINGS.
           PERFORM L3-TOTALS THRU L3-TOTALS-EXIT
               VARYING SUB-I FROM 1 BY 1
                   UNTIL SUB-I > MAX-TBL-SIZE.
           CLOSE PRTOUT1 PRTOUT2 FLATOUT1 FLATOUT2 FLATOUT3.
       L2-CLOSINGS-EXIT.
           EXIT.
           
       L3-OPENS.
           OPEN INPUT MACHINE-MASTER INVENTORY-DATA ORDER-DATA.
           OPEN OUTPUT PRTOUT1 PRTOUT2 FLATOUT1 FLATOUT2 FLATOUT3.
       L3-OPENS-EXIT.
           EXIT.
           
       L3-READS.
           READ MACHINE-MASTER.
           READ INVENTORY-DATA.
           READ ORDER-DATA.
           PERFORM L4-READ-MACH THRU L4-READ-MACH-EXIT
               UNTIL NO-MORE-RECS.
           MOVE 'Y' TO MORE-RECS.
           MOVE 1 TO SUB.
           PERFORM L4-READ-INV THRU L4-READ-INV-EXIT
               UNTIL NO-MORE-RECS.
           MOVE 'Y' TO MORE-RECS.
           MOVE 1 TO SUB.
           PERFORM L4-READ-ORDER THRU L4-READ-ORDER-EXIT
               UNTIL NO-MORE-RECS.
       L3-READS-EXIT.
           EXIT.
           
       L3-CLOSE-INPUTS.
           CLOSE MACHINE-MASTER INVENTORY-DATA ORDER-DATA.
       L3-CLOSE-INPUTS-EXIT.
           EXIT.
           
       L3-MACH-LOOP.
           SET IDX-INV TO 1.
           SEARCH INVENTORY-TABLE
               AT END DISPLAY 'ERROR - INVENTORY RECORD NOT FOUND'
               WHEN TBL-I-PART(IDX-INV) = TBL-M-PART(SUB-M)
                   PERFORM L4-DETAIL THRU L4-DETAIL-EXIT
           END-SEARCH.
           ADD 1 TO SUB-M.
       L3-MACH-LOOP-EXIT.
           EXIT.
           
       L3-TOTALS.
           IF TBL-I-SHORT(SUB-I) IS NEGATIVE
               MOVE TBL-I-PART(SUB-I) TO O-PART O-TOT-PART
               MOVE TBL-I-SHORT(SUB-I) TO O-SHORT O-TOT-SHT
               WRITE PARTSHT-LN END-WRITE
               WRITE RPT1-LN FROM D1
                   AFTER ADVANCING 1 LINE
                       AT EOP PERFORM L9-RPT1-HDGS
               END-WRITE
           END-IF.
       L3-TOTALS-EXIT.
           EXIT.
           
       L4-READ-MACH.
           MOVE I-MACHINE TO TBL-M-MACHINE(SUB).
           MOVE I-MACHINE-PART TO TBL-M-PART(SUB).
           MOVE I-PART-QTY TO TBL-M-QTY(SUB).
           ADD 1 TO SUB.
           READ MACHINE-MASTER
               AT END MOVE 'N' TO MORE-RECS.
       L4-READ-MACH-EXIT.
           EXIT.
           
       L4-READ-INV.
           MOVE I-INV-PART TO TBL-I-PART(SUB).
           MOVE I-INV-QTY TO TBL-I-QTY(SUB).
           ADD 1 TO SUB.
           READ INVENTORY-DATA
               AT END MOVE 'N' TO MORE-RECS.
       L4-READ-INV-EXIT.
           EXIT.
           
       L4-READ-ORDER.
           MOVE I-ORD-MACHINE TO TBL-O-MACHINE(SUB).
           MOVE I-ORD-NUM TO TBL-O-QTY(SUB).
           ADD 1 TO SUB.
           READ ORDER-DATA
               AT END MOVE 'N' TO MORE-RECS
               MOVE SPACES TO TBL-O-MACHINE(SUB).
       L4-READ-ORDER-EXIT.
           EXIT.
           
       L4-DETAIL.
           COMPUTE TBL-M-SHORT(SUB-M) = TBL-I-QTY(IDX-INV) -
               (TBL-O-QTY(SUB-O) * TBL-M-QTY(SUB-M)).
           IF TBL-M-SHORT(SUB-M) IS NEGATIVE
               PERFORM L5-MOVES THRU L5-MOVES-EXIT
               ADD TBL-M-SHORT(SUB-M) TO TBL-I-SHORT(IDX-INV)
           END-IF.
       L4-DETAIL-EXIT.
           EXIT.
           
       L5-MOVES.
               MOVE TBL-M-MACHINE(SUB-M) TO O-MACHINE O-MACH.
               MOVE TBL-M-PART(SUB-M) TO O-MACH-PART O-MACHINE-PART.
               MOVE TBL-M-SHORT(SUB-M) TO O-MACH-SHT O-MACH-PART-SHORT.
               WRITE RPT2-LN FROM D2
                   AFTER ADVANCING 1 LINE
                       AT EOP PERFORM L9-RPT2-HDGS.
               WRITE MACHSHT-LN.
       L5-MOVES-EXIT.
           EXIT.
           
       L9-RPT1-HDGS.
           ADD 1 TO C-PCTR1.
           MOVE C-PCTR1 TO O-PCTR.
           WRITE RPT1-LN FROM CO-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE RPT1-LN FROM RPT1-TITLE-LINE
               AFTER ADVANCING 1 LINE.
           WRITE RPT1-LN FROM RPT1-COL-HDG-LINE
               AFTER ADVANCING 2 LINES.
           WRITE RPT1-LN FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
       L9-RPT1-HDGS-EXIT.
           EXIT.
           
       L9-RPT2-HDGS.
           ADD 1 TO C-PCTR2.
           MOVE C-PCTR2 TO O-PCTR.
           WRITE RPT2-LN FROM CO-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE RPT2-LN FROM RPT2-TITLE-LINE
               AFTER ADVANCING 1 LINE.
           WRITE RPT2-LN FROM RPT2-COL-HDG-LINE
               AFTER ADVANCING 2 LINES.
           WRITE RPT2-LN FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
       L9-RPT2-HDGS-EXIT.
           EXIT.