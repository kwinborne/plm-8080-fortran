module plm_globs
    implicit none
    integer  :: sp,mp,mpp1,mstack,mvar,vartop,pstack(75),var(75),varc(256),fixv(75),fixc(75),prmask(5)
    logical :: failsf,compil
    
    integer :: v(446),vloc(107),vindx(13),c1(364),c1tri(243),prtb(129),prdtb(129),hdtb(129),prlen(129),contc(129),leftc(5),lefti(57), &
     contt(1),tripi(57),prind(107),nsy,nt,vlen,vil,c1w,c1l,nc1tri,prtbl,prdtbl,hdtbl,prlenl,concl,leftcl,leftil,contl,tripl,pril,&
     pack,token,identv,numbv,strv,divide,eofile,procv,semiv,decl,dov,endv,groupv,stmtv,slistv
    
    integer :: block(30),dopar(30),macblk(30),curblk,maxblk,blksym
    integer :: proctp(30)
    INTEGER :: SYMBOL(4000),SYMTOP,MAXSYM,SYMLOC,SYMLEN,SYMCNT,SYMABS,ACNT
    INTEGER :: HENTRY(127),HCODE
    INTEGER :: CONTRL(64)
    INTEGER :: VARB,INTR,PROC,LABEL,LITER
    INTEGER :: MSSG(77)
    INTEGER INTPRO(8)
end module plm_globs