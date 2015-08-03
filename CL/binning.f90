

module myModule
IMPLICIT DOUBLE PRECISION (A-H,O-Z)

data X_min/-6.d0/
data X_max/6.d0/
data Y_min/-6.d0/
data Y_max/6.d0/

data n_bins_x/150/,n_bins_y/150/
data n_data_pts/10000/

data cl_min/0.67d0/, cl_max/0.69d0/

end module myModule

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

program binning
use myModule
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
real*8 x_data(10000),y_data(10000),z_data(10000)
integer n_pts_bin(10000),n_bin_count(10000)
real*16 cl


! bin widths
 dx=(X_max-X_min)/n_bins_x
 dy=(y_max-y_min)/n_bins_y
 y_min_data=y_min

!open the original data file
 open(unit=20,file='BToXsgamma_ReC7_ReC7p.dat',status='old',action='read')
 read(20,*)(x_data(k),y_data(k),z_data(k),k=1,10000)
 close(20)

!data file to write the PDF
 open(unit=15,file='sorted.dat',status='unknown')
 open(unit=16,file='bin_counts.dat',status='unknown')

! start binning
 do i=1,n_bins_x+1
 x=x_min+dx
 do j=1,n_bins_y+1
 y=y_min+dy
	
 n_count=0
 do k=1,10000
 if(x_data(k).gt.x_min.and.x_data(k).lt.x .and. y_data(k).gt.y_min.and.y_data(k).lt.y)then
 n_count=n_count+1
 endif
 enddo
 write(16,*)n_count

 do k=1,10000
 if(x_data(k).gt.x_min.and.x_data(k).lt.x .and. y_data(k).gt.y_min.and.y_data(k).lt.y)then
 write(15,*)n_count,x_data(k),y_data(k)
 endif
 enddo
 

 y_min=y
 enddo
 y_min=y_min_data
 x_min=x
 enddo
 close(15) ! end binning...
 close(16)


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!read each bin..read the number of points and the cooredinates
 number_of_bins=(n_bins_x+1)*(n_bins_y+1)
 open(unit=20,file='bin_counts.dat',status='old',action='read')
 read(20,*)(n_pts_bin(k),k=1,number_of_bins)
 close(20) 

 open(unit=15,file='final_data_file.dat',status='unknown')
 cl=0.d0
! pick up the bins with largest number of points
 m=100000
 10 max_bin_count=0
 do k=1,number_of_bins
 if(n_pts_bin(k).lt.m)then
 max_bin_count=max(n_pts_bin(k),max_bin_count)
 endif
 enddo
 !write(*,*)max_bin_count

!now see how many times max bin appears
 n_times_max_bin_appears=0
 do k=1,number_of_bins
 if(max_bin_count.eq.n_pts_bin(k))then
 n_times_max_bin_appears=n_times_max_bin_appears+1
 endif
 enddo
 write(*,*)max_bin_count,n_times_max_bin_appears


!write down all the data that correspond to max_bin_count
 open(unit=20,file='sorted.dat',status='old',action='read')
 read(20,*)(n_bin_count(k),x_data(k),y_data(k),k=1,10000)
 close(20)

 do k=1,n_data_pts
 if(n_bin_count(k).eq.max_bin_count)then
 write(15,*)x_data(k),y_data(k),max_bin_count
 endif
 enddo

!check if max_bin_count*n_times_max_bin_appears is 68%
 cl=cl+max_bin_count*n_times_max_bin_appears/10000.d0
 write(*,*)cl
 if(cl .gt. cl_min .and. cl .lt. cl_max )then
 close(15)
 go to 100
 elseif(cl.gt. cl_max)then
 write(*,*)"beyond range...exiting"
 go to 100
 elseif(cl.lt.cl_min)then
 m=max_bin_count
 go to 10
 endif
 


100 stop
end
