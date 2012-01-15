data0 = load ("cross0.dat");
data1 = load ("cross1.dat");
data2 = load ("cross2.dat");
data3 = load ("cross3.dat");
data4 = load ("cross4.dat");
data5 = load ("cross5.dat");
data6 = load ("cross6.dat");
data7 = load ("cross7.dat");
data8 = load ("cross8.dat");
data9 = load ("cross9.dat");
data10 = load ("cross10.dat");

data0(:, (1:2)) = data0(:, (1:2)) ./ repmat(data0(:, 3), 1, 2);
data1(:, (1:2)) = data1(:, (1:2)) ./ repmat(data1(:, 3), 1, 2);
data2(:, (1:2)) = data2(:, (1:2)) ./ repmat(data2(:, 3), 1, 2);
data3(:, (1:2)) = data3(:, (1:2)) ./ repmat(data3(:, 3), 1, 2);
data4(:, (1:2)) = data4(:, (1:2)) ./ repmat(data4(:, 3), 1, 2);
data5(:, (1:2)) = data5(:, (1:2)) ./ repmat(data5(:, 3), 1, 2);
data6(:, (1:2)) = data6(:, (1:2)) ./ repmat(data6(:, 3), 1, 2);
data7(:, (1:2)) = data7(:, (1:2)) ./ repmat(data7(:, 3), 1, 2);
data8(:, (1:2)) = data8(:, (1:2)) ./ repmat(data8(:, 3), 1, 2);
data9(:, (1:2)) = data9(:, (1:2)) ./ repmat(data9(:, 3), 1, 2);
data10(:, (1:2)) = data10(:, (1:2)) ./ repmat(data10(:, 3), 1, 2);

figure;
hold on;
plot((1:(length (data0(:, 1)))), data0(:, 1), "y;cross0;");
plot((1:(length (data1(:, 1)))), data1(:, 1), "0;cross1;");
plot((1:(length (data2(:, 1)))), data2(:, 1), "1;cross2;");
plot((1:(length (data3(:, 1)))), data3(:, 1), "2;cross3;");
plot((1:(length (data4(:, 1)))), data4(:, 1), "3;cross4;");
plot((1:(length (data5(:, 1)))), data5(:, 1), "4;cross5;");
plot((1:(length (data6(:, 1)))), data6(:, 1), "5;cross6;");


xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 5");
print ("crossEvolution_0-6.png")
close;

figure;
hold on;
plot((1:(length (data3(:, 1)))), data3(:, 1), "2;cross3;");
plot((1:(length (data4(:, 1)))), data4(:, 1), "3;cross4;");
plot((1:(length (data5(:, 1)))), data5(:, 1), "4;cross5;");
plot((1:(length (data6(:, 1)))), data6(:, 1), "5;cross6;");
plot((1:(length (data7(:, 1)))), data7(:, 1), "y;cross7;");
plot((1:(length (data8(:, 1)))), data8(:, 1), "0;cross8;");
plot((1:(length (data9(:, 1)))), data9(:, 1), "1;cross0;");
plot((1:(length (data10(:, 1)))), data10(:, 1), "2;cross10;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 5");
print ("crossEvolution_3-10.png")
close;

