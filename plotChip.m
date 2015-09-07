data=dlmread('fieldLine.dat');
subplot(2,1,1)
plot(data(:,1),data(:,2))
axis equal
subplot(2,1,2)
plot(data(:,1),data(:,3))

