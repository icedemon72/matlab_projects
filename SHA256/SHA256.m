function [ out ] = sha256()
    % Konstante, predefinisane initialHash popunjava H array, a constK
    % popunjava W
    initialHash = ['6a09e667'; 'bb67ae85'; '3c6ef372'; 'a54ff53a'; '510e527f'; '9b05688c'; '1f83d9ab'; '5be0cd19'];
    constK = ['428a2f98'; '71374491'; 'b5c0fbcf'; 'e9b5dba5'; '3956c25b'; '59f111f1'; '923f82a4'; 'ab1c5ed5';
        'd807aa98'; '12835b01'; '243185be'; '550c7dc3'; '72be5d74'; '80deb1fe'; '9bdc06a7'; 'c19bf174';
        'e49b69c1'; 'efbe4786'; '0fc19dc6'; '240ca1cc'; '2de92c6f'; '4a7484aa'; '5cb0a9dc'; '76f988da';
        '983e5152'; 'a831c66d'; 'b00327c8'; 'bf597fc7'; 'c6e00bf3'; 'd5a79147'; '06ca6351'; '14292967';
        '27b70a85'; '2e1b2138'; '4d2c6dfc'; '53380d13'; '650a7354'; '766a0abb'; '81c2c92e'; '92722c85';
        'a2bfe8a1'; 'a81a664b'; 'c24b8b70'; 'c76c51a3'; 'd192e819'; 'd6990624'; 'f40e3585'; '106aa070';
        '19a4c116'; '1e376c08'; '2748774c'; '34b0bcb5'; '391c0cb3'; '4ed8aa4a'; '5b9cca4f'; '682e6ff3';
        '748f82ee'; '78a5636f'; '84c87814'; '8cc70208'; '90befffa'; 'a4506ceb'; 'bef9a3f7'; 'c67178f2'];

    userInput = input('unesi nes pls: ', 's');
    
    % Promenljive paddedLength i padded, obe se koriste u parse2blocks
    % funkciji radi generisanja blockNum blokova duzine 512
    [ paddedLength, padded ] = padding(userInput);
    [ blockNum, M ] = parse2blocks(padded, paddedLength);
    
    % Inicijalizacija praznih matrica W -> 64 x 32(bit) H -> 8 x 32(bit)
    W = zeros(64, 32);
    H = zeros(8, 32);
    
    % Popunjavanje H matrice
    for i = 1:8
        H(i, :) = hexToBinaryVector(initialHash(i, :), 32);
    end
    
    % Po formuli, svaki napravljeni blok treba da se obradi
    for i = 1:blockNum
        % U svakom bloku ima 64bit-a
        for j = 1:64
            % Prvih 16 redova rezervisano je za vrednosti u bloku M
            if(j > 0 && j <= 16)
                W(j, :) = M((32 * (j - 1) + 1):(j * 32));
            else
                % Sabiraju se binarno ?1(W(j-2)) + W(j-7) + ?2(j-15) + W(j-16)
                % i ta vrednost se dodeljuje redovima 17-64
                W(j, :) = addMsgSch(sigma1(W(j - 2, :)), W(j - 7, :), sigma0(W(j - 15, :)), W(j - 16, :));
            end
            
        end
        
        % Pravi se 8 promenljivih u koje se smestaju inicijalne vrednosti
        a = H(1, :);
        b = H(2, :);
        c = H(3, :);
        d = H(4, :);
        e = H(5, :);
        f = H(6, :);
        g = H(7, :);
        h = H(8, :);
        
        % 64 bit = 1 blok
        for t = 1:64
            % K je konstanta iz constK niza sa indeksom t u binarnom br sis
            K = hexToBinaryVector(constK(t, :), 32);
            %T1 = h + 
            temp1 = addMsgSch(h, cSigma1(e), choice(e, f, g), K, W(t, :));
            temp2 = addMsgSch(cSigma0(a), majority(a, b, c));
            h = g;
            g = f;
            f = e;
            e = addMsgSch(d, temp1);
            d = c;
            c = b;
            b = a;
            a = addMsgSch(temp1, temp2);
        end
        
        H(1, :) = addMsgSch(H(1, :), a);
        H(2, :) = addMsgSch(H(2, :), b);
        H(3, :) = addMsgSch(H(3, :), c);
        H(4, :) = addMsgSch(H(4, :), d);
        H(5, :) = addMsgSch(H(5, :), e);
        H(6, :) = addMsgSch(H(6, :), f);
        H(7, :) = addMsgSch(H(7, :), g);
        H(8, :) = addMsgSch(H(8, :), h);

        horz = horzcat(H(1,:), H(2,:), H(3,:), H(4,:), H(5,:), H(6,:) ,H(7,:), H(8,:));

        out = binaryVectorToHex(horz);
        
    end
    
end


function [ len, out ] = padding(input)
    len = length(input);
    padded = [];
    
    for i = 1:len
        padded = strcat(padded, dec2bin(double(input(i)), 8));
    end
        
    padded = strcat(padded, '1');
    
    k = 1;
    
    while(mod(length(padded) + k,  512) ~= 448)
        k = k + 1;
    end
    
    for i = 1:k 
        padded = strcat(padded, '0');
    end
    
    padded = strcat(padded, dec2bin(len * 8, 64));
                
    len = length(padded);
    out = logical(padded(:)'-'0');
end

function [ blockNum, out ] = parse2blocks(paddedInput, paddedLength) 
    blockNum = paddedLength / 512;
    
    out = zeros(blockNum, 512);
    
    for i = 1:blockNum
        for j = 1:512
            out(i, j) = paddedInput((i - 1) * 512 + j);
        end
    end
    
end

function [ out ] = addMsgSch ( varargin )
    out = 0;
    
    for i = 1:length(varargin)
        out = out + binaryVectorToDecimal(varargin{i});
    end
    
    out = dec2bin(mod(out, 2^32), 32);
    out = logical( out(:)'-'0' ); 
end

% FUNKCIJE PRILIKOM HASH KOMPUTACIJE %

function [ out ] = sigma0(w)
    fXOR = xor(rotBits(w, 7), rotBits(w, 18));
    out = xor(fXOR, shiftBits(w, 3));
end

function [ out ] = sigma1(w)
    fXOR = xor(rotBits(w, 17), rotBits(w, 19));
    out = xor(fXOR, shiftBits(w, 10));
end

function [ out ] = cSigma0(a)
    fXOR = xor(rotBits(a, 2), rotBits(a, 13));
    out = xor(fXOR, rotBits(a, 22));
end

function [ out ] = cSigma1(e)
    fXOR = xor(rotBits(e, 6), rotBits(e, 11));
    out = xor(fXOR, rotBits(e, 25));
end

function [ out ] = choice(e, f, g)
    fAND = and(e, f);
    sAND = and(not(e), g);
    out = xor(fAND, sAND);
end

function [ out ] = majority(a, b, c)
    fAND = and(a, b);
    sAND = and(a, c);
    tAND = and(b, c);
    fXOR = xor(fAND, sAND);
    out = xor(fXOR, tAND);
end

% FUNKCIJE ROTIRANJA BITOVA %

function [ out ] = rotBits(input, n)
    bitSize = 32;
    out = input;
    factor = 0;
    n = rem(n, bitSize);

    if(n < 0)
        n = n - 1;
    end
         
    if(n ~= 0)
        for i = 1:bitSize
            index = mod((i + n), (bitSize + 1)); 

            if(index == 0)
                factor = 1;
            end     
            
            out(index + factor) = input(i);          
        end
    end
end

function [ out ] = shiftBits(input, n)
    bitSize = 32;
    out = zeros(1, bitSize);
    
    if(abs(n) > bitSize)
        n = bitSize;
    end
         
    if(n ~= 0)
        if(n > 0)
            for i = 1:(bitSize - n);
                out(i + n) = input(i);
            end
        else
            for i = 1:(bitSize - abs(n));
                out(i) = input(i + abs(n));
            end
        end
    else
        out = input;
    end
    
end