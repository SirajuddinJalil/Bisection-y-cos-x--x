program akar_fungsi 
    implicit none
    double precision x1,x2,x3,x4,kesrel,kecil
    kecil=0.0001d0
    
    do
        write(*,*) "masukkan dua batas awal"
        read(*,*) x1,x2
        if (f(x1)*f(x2) < 0.d0) exit
    end do
        
!mencari akar
    x3=(x1+x2)/2.d0
    do 
        if (f(x3)*f(x2) < 0.d0) then
            x1 = x3
        else
            x2 = x3
        end if
        x4 = (x1+x2)/2.d0
        kesrel = abs((x3-x4)/x4)
        if (kesrel<kecil) exit
        x3=x4
    end do
    write(*,*) "nilai akarnya adlah=" , x4
    write (*,*) "nilai fungsinya=", f(x4)
    stop
        contains
        function f(x) result (y)
            implicit none
            double precision y
            double precision, intent(in) :: x
            y=cos(x)-x
            return
        end function f
end program akar_fungsi
