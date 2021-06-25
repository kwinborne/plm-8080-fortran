    program plm_cmp
    use plm_globs
    implicit none

    contains
    subroutine conout(cc,k,n,base)
    integer, intent(in) :: base, cc, k, n
    integer :: t(20)
    integer :: i, kp, np, j, l, ltemp, ip
    logical zsup
    np = n
    zsup = (k < 0)
    kp = imin (iabs(k),19)

    do i=1,kp
        t(i) = 1
    end do

    ip = kp + 1

    do i=1,kp
        ltemp=ip-i
        t(ltemp)=mod(np,base)+2
        np = np/base
        if(zsup .and. (np == 0)) exit
    end do

    select case (base)
    case(8)
        kp=kp+1
        t(kp)=28
    case(2)
        kp=kp+1
        t(kp)=13
        case default
        kp = kp+1
        t(kp) = 19
    end select

    call form(cc,t,1,kp,20)
    return
    end subroutine conout

    integer function imin(i,j)
    integer, intent(in) :: i, j
    if(i<j) then
        imin=j
    else
        imin=j
    end if
    RETURN
    end function imin

    INTEGER FUNCTION ENTER(INFOV)
    integer, intent(in) :: infov
    INTEGER :: Q,TYP,INFO,SHR,SHL,RIGHT
    INTEGER :: CONV,GETC1
    integer :: i, j, k, ihash, ltemp, ltemp1
    integer :: iq, l, ip
    !      ENTER ASSUMES A PREVIOUS CALL TO LOOKUP (EITHER THAT, OR SET UP
    !      THE VALUES OF SYMLOC AND SYMLEN IN THE VARC ARRAY).
    !         ALSO SET-UP HASH CODE VALUE (SEE LOOKUP), IF NECESSARY
    INFO = INFOV
    I = SYMTOP
    if(info>0) then
        IHASH = 1
        Q = (SYMLEN-1)/PACK + 1
    else
        ihash=0
        hcode = 0
        info = - info
        symlen = 0
        q = 0
    end if
    SYMTOP = SYMTOP + Q + IHASH + 3
    IQ = I
    I = I + IHASH

    if(symtop<maxsym)then
        SYMBOL(SYMTOP) = I
        SYMCNT = SYMCNT + 1
        SYMBOL(I) = SHL(SYMCNT,16) + SYMBOL(IQ)
        I = I + 1
        SYMBOL(I) = SHL(Q,12) + SYMLEN
        IP = I + 1
        SYMBOL(IP) = INFO
        L = SYMLOC - 1
    else
        i=ihash
        symtop = q+ihash+3
        call error(2,5)
    end if
    if(q/=0)then
        do j=1, q
            ltemp = ip+j
            ltemp1 = l+j
            symbol(ltemp)=varc(ltemp1)
        end do
    end if
    ENTER = I
    !     COMPUTE HASH TABLE ENTRY
    if(ihash /= 0)then
        symbol(iq) = shl(hcode,16) + hentry(hcode)
        hentry(hcode) = iq
    end if
    RETURN
    END function enter


    SUBROUTINE DUMPSY

    INTEGER :: RIGHT,SHR,SHL
    INTEGER :: LOOKUP,ENTER
    integer :: i, kp, lp, l, k, ic, ifin
    integer :: ltemp, j, mc, m, n
    integer :: ip, it, jp

    IC = CONTRL(30)
    
    if(.not. (ic == 0)) then
        !IF (IC.EQ.0) GO TO 2000
        CALL WRITEL(0)
        IF (IC.GT.1) CALL FORM(0,MSSG,42,77,77)
        I = SYMBOL(SYMTOP)
        IT = SYMTOP
        if (.not. (i < 0)) then
            do while (.not. (i<0))
                !210   if (i .le. 0) go to 1000
                k = symbol(i)
                kp = shr(k,16)
                !     quick check for zero length name
                
                if(.not.(ic > 2)) then
                    n = iabs(symbol(i+1))
                    if(.not.(shr(n,12)==0)) then
                    call pad(0,30,1)
                    call conout(1,5,kp,10)
                end if
            else
                call pad(0,30,1)
                call conout(1,5,kp,10)
            end if
                k = symbol(i+1)
                
                if(.not.(ic<2)) then
                    j=1
                    if(k<0) j = 47
                    call pad(1,j,1)
                    call pad(1,1,1)
                end if
                
                k = iabs(k)
                kp = shr(k,12)
                n = kp
                k = right(k,12)
                mc = k

                if(.not.(ic<2)) then
                    call conout(1,4,i+1,10)
                    call pad(1,1,1)
                    call conout(1,-3,kp,10)
                    call pad(1,1,1)
                    call conout(1,-4,k,10)
                    call pad(1,1,1)
                end if

                k = symbol(i+2)
                j = 29
                
                if(.not.(ic<2)) then
                    if(k<0)then
                        j=13
                    end if
                    call pad(1,j,1)
                    call pad(1,1,1)
                end if

                k = iabs(k)
                m = right(k,4)

                if(.not. (ic < 2))then
                    kp = shr(k,8)
                    call conout(1,6,kp,10)
                    kp = right(shr(k,4),4)
                    call conout(1,-3,kp,10)
                    kp = right(k,4)
                    call conout(1,-3,kp,10)
                end if

                call pad(1,1,1)
                ip = i+2
                if(.not.(n==0))then
                    if(m==liter) then
                        call pad(1,46,1)
                    end if
                    do kp=1,n
                        ltemp=kp+ip
                        l=symbol(ltemp)
                        do lp=1,pack
                            if ((kp-1)*pack+lp.gt.mc)then
                                exit
                            end if
                            jp = 30-lp*6
                            jp = right(shr(l,jp),6)+1
                            call pad(1,jp,1)
                        end do
                    end do
                end if
                if (m.eq.liter) call pad(1,46,1)
                ip = ip + n
                if(.not.(ic<2))then
                    ip = ip + 1
                end if
                if(.not. (ip > it)) then
                    do while (.not. (ip > it))
                        call pad(1,1,1)
                        k = symbol(ip)
                        j = 1
                        if (k < 0) then
                            j = 45
                        end if
                        call pad(1,j,1)
                        k = iabs(k)
                        call conout(1,8,k,16)
                        ip = ip + 1
                    end do
                end if
                it = i
                i = right(symbol(i),16)
            end do
        end if
        call writel(0)
    end if
    CALL WRITEL(0)
    K = CONTRL(26)
    CONTRL(26) = CONTRL(32)
    KP = CONTRL(34)
    CONTRL(34) = CONTRL(33)
    !     WRITE THE INTERRUPT PROCEDURE NAMES
    CALL PAD(1,41,1)
    do i = 1,8
        j = intpro(i)
        if(j < 0) cycle
        !         write intnumber symbolnum (4 base-32 digits)
        call pad(1,i+1,1)
        do  l=1,3
            call pad(1,right(j,5)+2,1)
            j = shr(j,5)
        end do
        call pad(1,41,1)
    end do
    
    CALL PAD(1,41,1)
    CALL WRITEL(0)
    !
    !    REVERSE THE SYMBOL TABLE POINTERS
    !    SET THE LENGTH FIELD OF COMPILER-GENERATED LABELS TO 1
    !
    L = 0
    I = SYMTOP
    J = SYMBOL(I)
    SYMBOL(I) = 0
    
    if(.not.(j==0))then
        do while (j /= 0)
            L = L + 1
            !     CHECK FOR A LABEL VARIABLE
            K = SYMBOL(J+2)
            if(mod(k,16)==label) k=iabs(symbol(j+1))
            if(mod(k,4096)==0) symbol(j+2) = 336 + label
            IF (MOD(K,16).NE.LABEL) GO TO 2110
            !     CHECK FOR CHARACTER LENGTH = 0
            !     SET LENGTH TO 1 AND PREC TO 5 (FOR COMP GENERATED LABELS)
            !         336 = 1 * 256 + 5 * 16
            M = SYMBOL(J)
            SYMBOL(J) = I
            I = J
            J = RIGHT(M,16)
        end do
    end if

        
2100 IF (J.EQ.0) GO TO 2200
    L = L + 1
    !     CHECK FOR A LABEL VARIABLE
    K = SYMBOL(J+2)
    IF (MOD(K,16).NE.LABEL) GO TO 2110
    !     CHECK FOR CHARACTER LENGTH = 0
    K = IABS(SYMBOL(J+1))
    IF (MOD(K,4096).NE.0) GO TO 2110
    !     SET LENGTH TO 1 AND PREC TO 5 (FOR COMP GENERATED LABELS)
    SYMBOL(J+2) = 336 + LABEL
    !         336 = 1 * 256 + 5 * 16
2110 M = SYMBOL(J)
     
     
    SYMBOL(J) = I
    I = J
    J = RIGHT(M,16)
    GO TO 2100
    !
2200 CONTINUE
    JP = 0
    IFIN = 1
    IP = 1
    J = 1
    !
2500 IF (J.NE.JP) GO TO 2610
    J = J + IP
2610 IF (J.LT.IFIN) GO TO 2700
    !     OTHERWISE GET ANOTHER ENTRY FROM TABLE
    CALL PAD(1,41,1)
    J = I + 1
    I = SYMBOL(I)
    IF (I.EQ.0) GO TO 2800
    IP = IABS(SYMBOL(J))
    IP =  RIGHT(SHR(IP,12),12)
    J = J + 1
    JP = J + 1
    !         CHECK FOR BASED VARIABLE -- COMPUTE LAST ENTRY
    IFIN = JP + IP
    IF (SYMBOL(J).LT.0) IFIN = IFIN + 1
    GO TO 2500
2700 L = 1
    LP = SYMBOL(J)
    IF (LP.LT.0) L = 45
    LP = IABS(LP)
    CALL PAD(1,L,1)
2710 CALL PAD(1,RIGHT(LP,5)+2,1)
    LP = SHR(LP,5)
    IF (LP.GT.0) GO TO 2710
    J = J + 1
    GO TO 2500

2800 CALL PAD(1,41,1)
    CALL WRITEL(0)
    CONTRL(26) = K
    CONTRL(34) = KP
    RETURN
    END subroutine dumpsy


    SUBROUTINE RECOV
    INTEGER GETC1
    INTEGER RIGHT
    integer :: i
    !     FIND SOMETHING SOLID IN THE TEXT
100 IF(TOKEN.EQ.DECL.OR.TOKEN.EQ.PROCV.OR.TOKEN.EQ.ENDV.OR.TOKEN.EQ.DOV.OR.TOKEN.EQ.SEMIV.OR.TOKEN.EQ.EOFILE) GO TO 300
200 CALL SCAN
    GO TO 100
    !     AND IN THE STACK
300 I = PSTACK(SP)
    IF (FAILSF.AND.GETC1(I,TOKEN).NE.0) GO TO 500
    IF (I.EQ.EOFILE.AND.TOKEN.EQ.EOFILE) GO TO 400
    IF ((I.EQ.GROUPV.OR.I.EQ.SLISTV.OR.I.EQ.STMTV.OR.I.EQ.DOV.OR.I.EQ.PROCV).AND.TOKEN.NE.EOFILE) GO TO 200
    !         BUT DON'T GO TOO FAR
    IF (SP.LE.4) GO TO 200
    VARTOP = RIGHT(VAR(SP),12)
    SP = SP - 1
    GO TO 300
400 COMPIL = .FALSE.
500 FAILSF = .FALSE.
    RETURN
    END subroutine recov

    end program plm_cmp
