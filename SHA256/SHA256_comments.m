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

    %userInput = input('unesi nes pls: ', 's');
    userInput = '123';
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
        % U svakom mini-bloku ima 64bit-a (16 * 64 = 512)
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
        
        % 64 bit = 1 mini-blok
        for t = 1:64
            % K je konstanta iz constK niza sa indeksom t u binarnom br sis
            K = hexToBinaryVector(constK(t, :), 32);
            % T1 = h + ?1(e) + Ch(e, f, g) + K + W(t)
            % T2 = ?0(a) + Maj(a, b, c)
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
        
        % Nakon 64 * blockNum iteracija, dobijeno je finalno resenje, gde
        % se u svakom H redu nalazi 32 bita koji ce uzeti heksadecimalnu
        % vrednost Hi = Hi + a ... h
        H(1, :) = addMsgSch(H(1, :), a);
        H(2, :) = addMsgSch(H(2, :), b);
        H(3, :) = addMsgSch(H(3, :), c);
        H(4, :) = addMsgSch(H(4, :), d);
        H(5, :) = addMsgSch(H(5, :), e);
        H(6, :) = addMsgSch(H(6, :), f);
        H(7, :) = addMsgSch(H(7, :), g);
        H(8, :) = addMsgSch(H(8, :), h);

        % Spajanje H1-8 u jednu matricu
        horz = horzcat(H(1,:), H(2,:), H(3,:), H(4,:), H(5,:), H(6,:) ,H(7,:), H(8,:));

        % Pretvaranje matrice u heksadecimalni broj
        out = binaryVectorToHex(horz);
        
    end
    
end

% Funkcija padding se koristi u prvom koraku, tj u preprocesovanju
function [ len, out ] = padding(input)
    len = length(input);
    padded = [];
    
    % 1. Za svaki karakter u input-u generise se 8bit-na vrednost
    for i = 1:len
        % 1.1 Vrednost se append-uje u padded char niz
        padded = strcat(padded, dec2bin(double(input(i)), 8));
    end
        
    % 2. Dodaje se jedinica
    padded = strcat(padded, '1');
    
    k = 1;
    
    % 3. Trazi se k po formuli len + k = 448mod512
    while(mod(length(padded) + k,  512) ~= 448)
        k = k + 1;
    end
    
    % 4. Dodaje se k nula
    for i = 1:k 
        padded = strcat(padded, '0');
    end
    
    % 5. Na kraju, dodaje se duzina inputa u 64bit formatu
    padded = strcat(padded, dec2bin(len * 8, 64));
                
    % out se konvertuje u logicki niz
    len = length(padded);
    out = logical(padded(:)'-'0');
end

% Funkcija koja vraca broj blokova od 512bit-a i njih same
function [ blockNum, out ] = parse2blocks(paddedInput, paddedLength) 
    % paddedLength je sigurno deljiv sa 512, pa blokove mozemo uzeti odatle
    blockNum = paddedLength / 512;
    out = zeros(blockNum, 512);
    
    % Popunjavamo M(i) blok
    for i = 1:blockNum
        for j = 1:512
            out(i, j) = paddedInput((i - 1) * 512 + j);
        end
    end
    
end

% Funkcija koja sabira brojeve u binarnom brojnom sistemu
% varargin je predefinisana funkcija koja je niz svih argumenata
function [ out ] = addMsgSch ( varargin )
    out = 0;
    
    for i = 1:length(varargin)
        out = out + binaryVectorToDecimal(varargin{i});
    end
    
    out = dec2bin(mod(out, 2^32), 32);
    out = logical( out(:)'-'0' ); 
end

% FUNKCIJE PRILIKOM HASH KOMPUTACIJE %
%(W(j-2)) + W(j-7) + ?2(j-15) + W(j-16)
% T1 = h + ?1(e) + Ch(e, f, g) + K + W(t)
            % T2 = ?0(a) + Maj(a, b, c)
            
% ?0(w) = w->>7 XOR w->>18 XOR w>>3
function [ out ] = sigma0(w)
    fXOR = xor(rotBits(w, 7), rotBits(w, 18));
    out = xor(fXOR, shiftBits(w, 3));
end
% ?1(w) = w->>17 XOR w->>19 XOR w>>10
function [ out ] = sigma1(w)
    fXOR = xor(rotBits(w, 17), rotBits(w, 19));
    out = xor(fXOR, shiftBits(w, 10));
end

% ?0(a) = a->>2 XOR a->>13 XOR a->>22
function [ out ] = cSigma0(a)
    fXOR = xor(rotBits(a, 2), rotBits(a, 13));
    out = xor(fXOR, rotBits(a, 22));
end

% ?1(e) = e->>6 XOR e->>11 XOR e->>25
function [ out ] = cSigma1(e)
    fXOR = xor(rotBits(e, 6), rotBits(e, 11));
    out = xor(fXOR, rotBits(e, 25));
end

% Ch(e, f, g) = (e AND f) XOR (NOT(e) AND g)
function [ out ] = choice(e, f, g)
    fAND = and(e, f);
    sAND = and(not(e), g);
    out = xor(fAND, sAND);
end

% Maj(a, b, c) = (a AND b) XOR (a AND c) XOR (b AND c)
function [ out ] = majority(a, b, c)
    fAND = and(a, b);
    sAND = and(a, c);
    tAND = and(b, c);
    fXOR = xor(fAND, sAND);
    out = xor(fXOR, tAND);
end

% FUNKCIJE ROTIRANJA BITOVA %
% Funckija koja rotira input bitove od bitSize za n mesta 
% (ukoliko je n < 0 onda je rotiranje ulevo)
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

% Funkcija koja shift-uje input bitove od bitSize za n mesta
% (ukoliko je n < 0 onda je rotiranje ulevo)
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