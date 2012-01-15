data0_0 = load ("mut0_0.dat");
data1_0 = load ("mut1_0.dat");
data2_0 = load ("mut2_0.dat");
data3_0 = load ("mut3_0.dat");
data4_0 = load ("mut4_0.dat");
data5_0 = load ("mut5_0.dat");
data6_0 = load ("mut6_0.dat");
data7_0 = load ("mut7_0.dat");
data8_0 = load ("mut8_0.dat");
data9_0 = load ("mut9_0.dat");
data10_0 = load ("mut10_0.dat");

data0_5 = load ("mut0_5.dat");
data1_5 = load ("mut1_5.dat");
data2_5 = load ("mut2_5.dat");
data3_5 = load ("mut3_5.dat");
data4_5 = load ("mut4_5.dat");
data5_5 = load ("mut5_5.dat");
data6_5 = load ("mut6_5.dat");
data7_5 = load ("mut7_5.dat");
data8_5 = load ("mut8_5.dat");
data9_5 = load ("mut9_5.dat");
data10_5 = load ("mut10_5.dat");

data0_0(:, (1:2)) = data0_0(:, (1:2)) ./ repmat(data0_0(:, 3), 1, 2);
data1_0(:, (1:2)) = data1_0(:, (1:2)) ./ repmat(data1_0(:, 3), 1, 2);
data2_0(:, (1:2)) = data2_0(:, (1:2)) ./ repmat(data2_0(:, 3), 1, 2);
data3_0(:, (1:2)) = data3_0(:, (1:2)) ./ repmat(data3_0(:, 3), 1, 2);
data4_0(:, (1:2)) = data4_0(:, (1:2)) ./ repmat(data4_0(:, 3), 1, 2);
data5_0(:, (1:2)) = data5_0(:, (1:2)) ./ repmat(data5_0(:, 3), 1, 2);
data6_0(:, (1:2)) = data6_0(:, (1:2)) ./ repmat(data6_0(:, 3), 1, 2);
data7_0(:, (1:2)) = data7_0(:, (1:2)) ./ repmat(data7_0(:, 3), 1, 2);
data8_0(:, (1:2)) = data8_0(:, (1:2)) ./ repmat(data8_0(:, 3), 1, 2);
data9_0(:, (1:2)) = data9_0(:, (1:2)) ./ repmat(data9_0(:, 3), 1, 2);
data10_0(:, (1:2)) = data10_0(:, (1:2)) ./ repmat(data10_0(:, 3), 1, 2);

data0_5(:, (1:2)) = data0_5(:, (1:2)) ./ repmat(data0_5(:, 3), 1, 2);
data1_5(:, (1:2)) = data1_5(:, (1:2)) ./ repmat(data1_5(:, 3), 1, 2);
data2_5(:, (1:2)) = data2_5(:, (1:2)) ./ repmat(data2_5(:, 3), 1, 2);
data3_5(:, (1:2)) = data3_5(:, (1:2)) ./ repmat(data3_5(:, 3), 1, 2);
data4_5(:, (1:2)) = data4_5(:, (1:2)) ./ repmat(data4_5(:, 3), 1, 2);
data5_5(:, (1:2)) = data5_5(:, (1:2)) ./ repmat(data5_5(:, 3), 1, 2);
data6_5(:, (1:2)) = data6_5(:, (1:2)) ./ repmat(data6_5(:, 3), 1, 2);
data7_5(:, (1:2)) = data7_5(:, (1:2)) ./ repmat(data7_5(:, 3), 1, 2);
data8_5(:, (1:2)) = data8_5(:, (1:2)) ./ repmat(data8_5(:, 3), 1, 2);
data9_5(:, (1:2)) = data9_5(:, (1:2)) ./ repmat(data9_5(:, 3), 1, 2);
data10_5(:, (1:2)) = data10_5(:, (1:2)) ./ repmat(data10_5(:, 3), 1, 2);

figure;
hold on;
plot((1:(length (data0_0(:, 1)))), data0_0(:, 1), "0;mut0;");
plot((1:(length (data1_0(:, 1)))), data1_0(:, 1), "1;mut1;");
plot((1:(length (data2_0(:, 1)))), data2_0(:, 1), "2;mut2;");
plot((1:(length (data3_0(:, 1)))), data3_0(:, 1), "3;mut3;");
plot((1:(length (data4_0(:, 1)))), data4_0(:, 1), "4;mut4;");
plot((1:(length (data5_0(:, 1)))), data5_0(:, 1), "5;mut5;");
plot((1:(length (data6_0(:, 1)))), data6_0(:, 1), "y;mut6;");
xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 0");
print ("mutEvolution_0-0-6.png")
close;

figure;
hold on;
plot((1:(length (data0_5(:, 1)))), data0_5(:, 1), "0;mut0;");
plot((1:(length (data1_5(:, 1)))), data1_5(:, 1), "1;mut1;");
plot((1:(length (data2_5(:, 1)))), data2_5(:, 1), "2;mut2;");
plot((1:(length (data3_5(:, 1)))), data3_5(:, 1), "3;mut3;");
plot((1:(length (data4_5(:, 1)))), data4_5(:, 1), "4;mut4;");
plot((1:(length (data5_5(:, 1)))), data5_5(:, 1), "5;mut5;");
plot((1:(length (data6_5(:, 1)))), data6_5(:, 1), "y;mut6;");
xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 5");
print ("mutEvolution_5-0-6.png")
close;

figure;
hold on;
plot((1:(length (data4_0(:, 1)))), data4_0(:, 1), "4;mut4;");
plot((1:(length (data5_0(:, 1)))), data5_0(:, 1), "5;mut5;");
plot((1:(length (data6_0(:, 1)))), data6_0(:, 1), "y;mut6;");
plot((1:(length (data7_0(:, 1)))), data7_0(:, 1), "0;mut7;");
plot((1:(length (data8_0(:, 1)))), data8_0(:, 1), "1;mut8;");
plot((1:(length (data9_0(:, 1)))), data9_0(:, 1), "2;mut9;");
plot((1:(length (data10_0(:, 1)))), data10_0(:, 1), "3;mut10;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 0");
print ("mutEvolution_0-4-10.png")
close;

figure;
hold on;
plot((1:(length (data4_5(:, 1)))), data4_5(:, 1), "4;mut4;");
plot((1:(length (data5_5(:, 1)))), data5_5(:, 1), "5;mut5;");
plot((1:(length (data6_5(:, 1)))), data6_5(:, 1), "y;mut6;");
plot((1:(length (data7_5(:, 1)))), data7_5(:, 1), "0;mut7;");
plot((1:(length (data8_5(:, 1)))), data8_5(:, 1), "1;mut8;");
plot((1:(length (data9_5(:, 1)))), data9_5(:, 1), "2;mut9;");
plot((1:(length (data10_5(:, 1)))), data10_5(:, 1), "3;mut10;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 5");
print ("mutEvolution_5-4-10.png")
close;

