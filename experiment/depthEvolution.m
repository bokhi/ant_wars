data1_0 = load ("depth1_0.dat");
data2_0 = load ("depth2_0.dat");
data3_0 = load ("depth3_0.dat");
data4_0 = load ("depth4_0.dat");
data5_0 = load ("depth5_0.dat");
data6_0 = load ("depth6_0.dat");
data7_0 = load ("depth7_0.dat");
data8_0 = load ("depth8_0.dat");
data9_0 = load ("depth9_0.dat");
data10_0 = load ("depth10_0.dat");

data1_5 = load ("depth1_5.dat");
data2_5 = load ("depth2_5.dat");
data3_5 = load ("depth3_5.dat");
data4_5 = load ("depth4_5.dat");
data5_5 = load ("depth5_5.dat");
data6_5 = load ("depth6_5.dat");
data7_5 = load ("depth7_5.dat");
data8_5 = load ("depth8_5.dat");
data9_5 = load ("depth9_5.dat");
data10_5 = load ("depth10_5.dat");

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
plot((1:(length (data1_0(:, 1)))), data1_0(:, 1), "1;depth1;");
plot((1:(length (data2_0(:, 1)))), data2_0(:, 1), "2;depth2;");
plot((1:(length (data3_0(:, 1)))), data3_0(:, 1), "3;depth3;");
plot((1:(length (data4_0(:, 1)))), data4_0(:, 1), "4;depth4;");
plot((1:(length (data5_0(:, 1)))), data5_0(:, 1), "5;depth5;");
plot((1:(length (data6_0(:, 1)))), data6_0(:, 1), "y;depth6;");
xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 0");
print ("depthEvolution_0-1-6.png")
close;

figure;
hold on;
plot((1:(length (data1_5(:, 1)))), data1_5(:, 1), "1;depth1;");
plot((1:(length (data2_5(:, 1)))), data2_5(:, 1), "2;depth2;");
plot((1:(length (data3_5(:, 1)))), data3_5(:, 1), "3;depth3;");
plot((1:(length (data4_5(:, 1)))), data4_5(:, 1), "4;depth4;");
plot((1:(length (data5_5(:, 1)))), data5_5(:, 1), "5;depth5;");
plot((1:(length (data6_5(:, 1)))), data6_5(:, 1), "y;depth6;");
xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 5");
print ("depthEvolution_5-1-6.png")
close;

figure;
hold on;
plot((1:(length (data4_0(:, 1)))), data4_0(:, 1), "4;depth4;");
plot((1:(length (data5_0(:, 1)))), data5_0(:, 1), "5;depth5;");
plot((1:(length (data6_0(:, 1)))), data6_0(:, 1), "y;depth6;");
plot((1:(length (data7_0(:, 1)))), data7_0(:, 1), "0;depth7;");
plot((1:(length (data8_0(:, 1)))), data8_0(:, 1), "1;depth8;");
plot((1:(length (data9_0(:, 1)))), data9_0(:, 1), "2;depth9;");
plot((1:(length (data10_0(:, 1)))), data10_0(:, 1), "3;depth10;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 0");
print ("depthEvolution_0-4-10.png")
close;

figure;
hold on;
plot((1:(length (data4_5(:, 1)))), data4_5(:, 1), "4;depth4;");
plot((1:(length (data5_5(:, 1)))), data5_5(:, 1), "5;depth5;");
plot((1:(length (data6_5(:, 1)))), data6_5(:, 1), "y;depth6;");
plot((1:(length (data7_5(:, 1)))), data7_5(:, 1), "0;depth7;");
plot((1:(length (data8_5(:, 1)))), data8_5(:, 1), "1;depth8;");
plot((1:(length (data9_5(:, 1)))), data9_5(:, 1), "2;depth9;");
plot((1:(length (data10_5(:, 1)))), data10_5(:, 1), "3;depth10;");

xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evolution of the average amount of food collected per game per generation - popSize 500 tourSize 10 crossRate 5");
print ("depthEvolution_5-4-10.png")
close;

