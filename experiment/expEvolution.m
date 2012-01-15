data0_0 = load ("exp0_0.dat");
data1_0 = load ("exp1_0.dat");
data2_0 = load ("exp2_0.dat");
data3_0 = load ("exp3_0.dat");
data4_0 = load ("exp4_0.dat");
data5_0 = load ("exp5_0.dat");
data6_0 = load ("exp6_0.dat");
data7_0 = load ("exp7_0.dat");
data8_0 = load ("exp8_0.dat");
data9_0 = load ("exp9_0.dat");
data10_0 = load ("exp10_0.dat");

data0_5 = load ("exp0_5.dat");
data1_5 = load ("exp1_5.dat");
data2_5 = load ("exp2_5.dat");
data3_5 = load ("exp3_5.dat");
data4_5 = load ("exp4_5.dat");
data5_5 = load ("exp5_5.dat");
data6_5 = load ("exp6_5.dat");
data7_5 = load ("exp7_5.dat");
data8_5 = load ("exp8_5.dat");
data9_5 = load ("exp9_5.dat");
data10_5 = load ("exp10_5.dat");

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
plot((1:(length (data0_0(:, 1)))), data0_0(:, 1), "0;exp0;");
plot((1:(length (data1_0(:, 1)))), data1_0(:, 1), "1;exp1;");
plot((1:(length (data2_0(:, 1)))), data2_0(:, 1), "2;exp2;");
plot((1:(length (data3_0(:, 1)))), data3_0(:, 1), "3;exp3;");
plot((1:(length (data4_0(:, 1)))), data4_0(:, 1), "4;exp4;");
plot((1:(length (data5_0(:, 1)))), data5_0(:, 1), "5;exp5;");
plot((1:(length (data6_0(:, 1)))), data6_0(:, 1), "y;exp6;");
xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 0");
print ("expEvolution_0-0-6.png")
close;

figure;
hold on;
plot((1:(length (data0_5(:, 1)))), data0_5(:, 1), "0;exp0;");
plot((1:(length (data1_5(:, 1)))), data1_5(:, 1), "1;exp1;");
plot((1:(length (data2_5(:, 1)))), data2_5(:, 1), "2;exp2;");
plot((1:(length (data3_5(:, 1)))), data3_5(:, 1), "3;exp3;");
plot((1:(length (data4_5(:, 1)))), data4_5(:, 1), "4;exp4;");
plot((1:(length (data5_5(:, 1)))), data5_5(:, 1), "5;exp5;");
plot((1:(length (data6_5(:, 1)))), data6_5(:, 1), "y;exp6;");
xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 5");
print ("expEvolution_5-0-6.png")
close;

figure;
hold on;
plot((1:(length (data4_0(:, 1)))), data4_0(:, 1), "4;exp4;");
plot((1:(length (data5_0(:, 1)))), data5_0(:, 1), "5;exp5;");
plot((1:(length (data6_0(:, 1)))), data6_0(:, 1), "y;exp6;");
plot((1:(length (data7_0(:, 1)))), data7_0(:, 1), "0;exp7;");
plot((1:(length (data8_0(:, 1)))), data8_0(:, 1), "1;exp8;");
plot((1:(length (data9_0(:, 1)))), data9_0(:, 1), "2;exp9;");
plot((1:(length (data10_0(:, 1)))), data10_0(:, 1), "3;exp10;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 0");
print ("expEvolution_0-4-10.png")
close;

figure;
hold on;
plot((1:(length (data4_5(:, 1)))), data4_5(:, 1), "4;exp4;");
plot((1:(length (data5_5(:, 1)))), data5_5(:, 1), "5;exp5;");
plot((1:(length (data6_5(:, 1)))), data6_5(:, 1), "y;exp6;");
plot((1:(length (data7_5(:, 1)))), data7_5(:, 1), "0;exp7;");
plot((1:(length (data8_5(:, 1)))), data8_5(:, 1), "1;exp8;");
plot((1:(length (data9_5(:, 1)))), data9_5(:, 1), "2;exp9;");
plot((1:(length (data10_5(:, 1)))), data10_5(:, 1), "3;exp10;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 5");
print ("expEvolution_5-4-10.png")
close;

