data10 = load ("pop10.dat");
data50 = load ("pop50.dat");
data100 = load ("pop100.dat");
data250 = load ("pop250.dat");
data500 = load ("pop500.dat");
data750 = load ("pop750.dat");
data1000 = load ("pop1000.dat");

data10(:, (1:2)) = data10(:, (1:2)) ./ repmat(data10(:, 3), 1, 2);
data50(:, (1:2)) = data50(:, (1:2)) ./ repmat(data50(:, 3), 1, 2);
data100(:, (1:2)) = data100(:, (1:2)) ./ repmat(data100(:, 3), 1, 2);
data250(:, (1:2)) = data250(:, (1:2)) ./ repmat(data250(:, 3), 1, 2);
data500(:, (1:2)) = data500(:, (1:2)) ./ repmat(data500(:, 3), 1, 2);
data750(:, (1:2)) = data750(:, (1:2)) ./ repmat(data750(:, 3), 1, 2);
data1000(:, (1:2)) = data1000(:, (1:2)) ./ repmat(data1000(:, 3), 1, 2);

figure;
hold on;
plot((1:(length (data10(:, 1)))), data10(:, 1), "0;pop10;");
plot((1:(length (data50(:, 1)))), data50(:, 1), "1;pop50;");
plot((1:(length (data100(:, 1)))), data100(:, 1), "2;pop100;");
plot((1:(length (data250(:, 1)))), data250(:, 1), "3;pop250;");
plot((1:(length (data500(:, 1)))), data500(:, 1), "4;pop500;");
plot((1:(length (data750(:, 1)))), data750(:, 1), "5;pop750;");
plot((1:(length (data100(:, 1)))), data100(:, 1), "g;pop1000;");
xlabel ("generation");
ylabel ("average amount of food collected per game");
legend("location", "northwest");
legend("right");
title ("evoluation of the average amount of food collected per game per generation");
