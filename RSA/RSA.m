%RSA algoritam
function [ out ] = RSA()

    % Ulazne predefinisane promenljive p i q
    % koje bi trebalo da budu prosti brojevi
    p = 2437;
    q = 5569;

    % n jeste maksimalni broj koji se moze enkriptovati
    % RSA algoritmom
    n = p * q;

    % e promenljiva treba da pripada opsegu 1 < e < f
    e = 65537;
    % istovremeno, e i fi trebaju biti coprime (relativno prosti) brojevi
    % tj. ne smeju biti deljivi istim brojevima (osim jedinice)
    f = (p - 1) * (q - 1); % FI

    % d je privatna eksponenta
    d = generatePrivate(e, f);
    
    %%%%%% USER GENERATED %%%%%%

    text = '12323152rsdasdasda';
    a = double(text);

    res_encr = zeros(1, length(a));
    res_decr = zeros(1, length(a));
    for i = 1:length(a)
        res_encr(i) = rem(sym(a(i))^e, n);
        res_decr(i) = rem(sym(res_encr(i))^d, n);
    end


    fprintf('\n\nUnos: %s', text);
    fprintf('\nEnkriptovano: ');
    fprintf('%d ', res_encr(1:end));
    fprintf('\nDekriptovano: ');
    fprintf('%d ', res_decr(1:end));
    fprintf('\n\n');

end

% Funkcija za generisanje privatne eksponente, kao argumente uzima
% e i mod (fi)
function [ out ] = generatePrivate(e, mod) 
    out = 1;

    % Dok god se ne pronadje broj D koji kada se pomnozi sa e i
    % podeli sa mod daje ostatak 1 [POSTOJI OPASNOST OD INF LOOP??]
    while(rem((e * out), mod) ~= 1)
        out = out + 1;
    end
end

% FUNKCIJE KOJE SE KORISTE ZA PROVERU PARAMETARA PRE UNOSA %

function [ out ] = isPrime(number)
    out = 1;
    if (number <= 3)
        out = 1;
    elseif (rem(number, 2) == 0)
        out = 0;
    else
        for i = 3:floor(number / 2)
            if(rem(number, i) == 0)
                out = 0;
                break
            end
        end
    end
end

% Funkcija koja proverava da li je broj relativno prost
function [ out ] = isRelPrime(a, b)
    out = 1;
    smaller = a;

    % Brojevi koji se uzimaju su manji od polovine najmanjeg broja
    if(a > b) 
        smaller = b;
    end

    % Ukoliko ima ostatka medjusobnog deljenja ili ako su oba parna
    if(rem(a, b) == 0 || (rem(a, 2) == 0 && rem(b, 2) == 0))
        out = 0;
    else
        for i = 3:floor(smaller / 2)
            if(rem(smaller, i) == 0) 
                out = 0;
                break;
            end
        end
    end
end
