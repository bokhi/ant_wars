data1 = load ("tour1.dat");
data2 = load ("tour2.dat");
data3 = load ("tour3.dat");
data4 = load ("tour4.dat");
data5 = load ("tour5.dat");
data6 = load ("tour6.dat");
data7 = load ("tour7.dat");
data8 = load ("tour8.dat");
data9 = load ("tour9.dat");
data10 = load ("tour10.dat");
data11 = load ("tour11.dat");
data12 = load ("tour12.dat");

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
data11(:, (1:2)) = data11(:, (1:2)) ./ repmat(data11(:, 3), 1, 2);
data12(:, (1:2)) = data12(:, (1:2)) ./ repmat(data12(:, 3), 1, 2);

figure;
hold on;
plot((1:(length (data1(:, 1)))), data1(:, 1), "0;tour1;");
plot((1:(length (data2(:, 1)))), data2(:, 1), "1;tour2;");
plot((1:(length (data3(:, 1)))), data3(:, 1), "2;tour3;");
plot((1:(length (data3(:, 1)))), data3(:, 1), "3;tour3;");
plot((1:(length (data4(:, 1)))), data4(:, 1), "4;tour4;");
plot((1:(length (data5(:, 1)))), data5(:, 1), "5;tour5;");
plot((1:(length (data6(:, 1)))), data6(:, 1), "y;tour6;");
plot((1:(length (data7(:, 1)))), data7(:, 1), "0;tour7;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evoluation of the average amount of food collected per game per generation - popSize 500");
print ("tourEvolution_2-7.png")
close;

figure;
hold on;
plot((1:(length (data4(:, 1)))), data4(:, 1), "4;tour4;");
plot((1:(length (data5(:, 1)))), data5(:, 1), "5;tour5;");
plot((1:(length (data6(:, 1)))), data6(:, 1), "y;tour6;");
plot((1:(length (data7(:, 1)))), data7(:, 1), "0;tour7;");
plot((1:(length (data8(:, 1)))), data8(:, 1), "1;tour8;");
plot((1:(length (data9(:, 1)))), data9(:, 1), "2;tour9;");
plot((1:(length (data10(:, 1)))), data10(:, 1), "3;tour10;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evoluation of the average amount of food collected per game per generation - popSize 500");
print ("tourEvolution_4-10.png")
close;

figure;
hold on;
plot((1:(length (data6(:, 1)))), data6(:, 1), "y;tour6;");
plot((1:(length (data7(:, 1)))), data7(:, 1), "0;tour7;");
plot((1:(length (data8(:, 1)))), data8(:, 1), "1;tour8;");
plot((1:(length (data9(:, 1)))), data9(:, 1), "2;tour9;");
plot((1:(length (data10(:, 1)))), data10(:, 1), "3;tour10;");
plot((1:(length (data11(:, 1)))), data11(:, 1), "4;tour11;");
plot((1:(length (data12(:, 1)))), data12(:, 1), "5;tour12;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evoluation of the average amount of food collected per game per generation - popSize 500");
print ("tourEvolution_6-12.png")
close;

