!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! n_points = number of data points
! n_bins = number of bins, => 0
! confidence_limit = the CL required
! n_1st_2nd_column = a switch to chose between the 1st or 2nd column that correspond to upper or lower bounds
subroutine binning1D(solutions, n_points, n_bins, confidence_limit, up, dn)
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
real*8 solutions(n_points), x_data(n_points)!,data1(n_points),data2(n_points)
integer n_pts_bin(n_points),n_bin_count(n_points)
real*8 cl

	
! read the unbinned data file and determine the max and min
x_max = -999.d0 ! these are some dummy numbers to pick up the max/min
x_min = 999.d0
	
 do i=1,n_points
 x_data(i)=solutions(i)
 x_max=max(x_max,solutions(i))
 x_min=min(x_min,solutions(i))
 enddo



! bin widths
 n_bins=n_bins+1
 dx=(X_max-X_min)/n_bins

	
!data file to write the histogram
! open(unit=16,file='bin_counts.dat',status='unknown')

! start binning
 do  i=1,n_bins   ! EARLIER THIS WAS 0 to n_bins
 x=x_min+dx
	
 n_count=0 ! n_count = number of points in a bin
 do k=1,n_points
 if(x_data(k).ge.x_min.and.x_data(k).lt.x)then
 n_count=n_count+1
 endif
 enddo
! write(16,*)n_count
 n_pts_bin(i)=n_count

! write the number of points in bin and the corresponding points
 do k=1,n_points
 if(x_data(k).ge.x_min.and.x_data(k).lt.x)then
 n_bin_count(k)=n_count
 x_data(k)=x_data(k)
 endif
 enddo
 
 x_min=x
 enddo
 close(15) ! end binning...
 close(16)

	

! NOW START TO CALCULATE THE CONFIDANCE LIMIT

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!
 cl=0.d0
 up = -999.d0
 dn = 999.d0
! pick up the bins with largest number of points
 m=12345678 ! some big number
 10 max_bin_count=0
 do k=1,n_bins
 if(n_pts_bin(k).lt.m)then
 max_bin_count=max(n_pts_bin(k),max_bin_count)
 endif
 enddo

	

!now see how many times max bin appears
 n_times_max_bin_appears=0
 do k=1,n_bins
 if(max_bin_count.eq.n_pts_bin(k))then
 n_times_max_bin_appears=n_times_max_bin_appears+1
 endif
 enddo
 

 do k=1,n_points
 if(n_bin_count(k).eq.max_bin_count)then
! write(15,*)x_data(k),max_bin_count
 up = max(up,x_data(k))
 dn = min(dn,x_data(k))
 endif
 enddo


!check if max_bin_count*n_times_max_bin_appears is 68%
 cl_min = confidence_limit-0.01d0
 cl_max = confidence_limit+0.01d0
 real_points = real(n_points,8)
 cl=cl+max_bin_count*n_times_max_bin_appears/real_points 

 if(cl.lt.cl_min)then
 m=max_bin_count
 go to 10
	
 elseif(cl .gt. cl_min .and. cl .lt. cl_max )then
 close(15)
 go to 100


 elseif(cl.gt. cl_max)then
 write(*,1)
 stop
 go to 100
 endif
 

1  FORMAT(//5X,'****WARNING**** The desired confidence limit can not be achived.', &
   '',/5X,'Change the number of bins.',//)


100 return
end
