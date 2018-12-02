open Core;

let hasExactly = (line, ~count) => {
  let m = Char.Map.empty;
  line
  |> String.to_list
  |> List.fold_left(~init=m, ~f=(map, char) =>
       Map.update(map, char, ~f=count =>
         switch (count) {
         | None => 1
         | Some(x) => x + 1
         }
       )
     )
  |> Map.data
  |> List.exists(~f=x => x == count);
};

let countHasExactly = (lines, ~count) =>
  List.count(lines, ~f=line => hasExactly(line, ~count));

let parseInput = input =>
  input
  |> String.split_on_chars(~on=[',', ' ', '\n'])
  |> List.map(~f=String.strip)
  |> List.filter(~f=x => x != "");

let checkSum = input => {
  let i = parseInput(input);
  let result = countHasExactly(i, ~count=2) * countHasExactly(i, ~count=3);
  Printf.printf("result: %i", result);
  result;
};

let oneDiff = (a, b) => {
  let aList = String.to_list(a);
  let bList = String.to_list(b);
  let rec search = (a, b, result, found) =>
    switch (a, b, found) {
    | ([], [], false) => None
    | ([], [], true) =>
      result |> List.rev |> String.of_char_list |> Option.some
    | ([cA, ...tailA], [cB, ...tailB], found) =>
      if (cA == cB) {
        search(tailA, tailB, [cA, ...result], found);
      } else if (found) {
        None;
      } else {
        search(tailA, tailB, result, true);
      }
    | _ => None
    };
  search(aList, bList, [], false);
};

let exactlyOneChange = (boxId, lines) => {
  let rec search = linesLeft =>
    switch (linesLeft) {
    | [] => None
    | [otherBoxId, ...tail] =>
      switch (oneDiff(boxId, otherBoxId)) {
      | Some(result) => Some(result)
      | None => search(tail)
      }
    };
  search(lines);
};

let commonBoxIds = input => {
  let i = parseInput(input);
  let resultO = List.find_map(~f=x => exactlyOneChange(x, i), i);
  switch (resultO) {
  | None => ""
  | Some(x) =>
    Printf.printf("common: %s\n", x);
    x;
  };
};

let%test "day2:star1:example" = {
  let testInput = "abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab
";
  checkSum(testInput) == 12;
};

let%test "day2:star2:example" = {
  let testInput = "abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz";

  commonBoxIds(testInput) == "fgij";
};
let input = "rmyxgdlihczskunpfwbgqoeybv
rmyxgdlksczskunpfwbjqkeatv
rmybgdxibczskunpfwbjqoeatv
rmyxgdlirczskuopfwbjqzeatv
rmyxedlrhczskunpfwbyqoeatv
rmyxfdlicczskunpfwbxqoeatv
rmyxgvlihkzskunpfwbsqoeatv
rmyxgdaihczvkunpfwblqoeatv
nmyxgolihczskunpfwbjqieatv
rhyxgdcihczskunifwbjqoeatv
rmfxgdlihczskunpfwbvqgeatv
smyxgdlihczskunsiwbjqoeatv
rmyxgdcihcxskunpfwbrqoeatv
rmyxgdlihczckuiqfwbjqoeatv
rmyxxdwihczskunifwbjqoeatv
rkzxgdlihczskunpfwhjqoeatv
rmypgdlihczskunpfwbrqoeafv
rmyxgplihczvkunpkwbjqoeatv
rqyxgdlihdzskjnpfwbjqoeatv
rmyxgdlihczskqnpswbjqoeaov
mcyxgdlihczmkunpfwbjqoeatv
rmyxgdlohczspunpowbjqoeatv
tmyxgdlihczskunpfwbeqoeltv
rmyxgdlibccskunpfwbjqoegtv
rmyxgdlehczsaunpfwboqoeatv
rmaxgdlihczseunpfwbjqojatv
rmyxgdlijczskynpfwbjboeatv
kmlxgdlilczskunpfwbjqoeatv
rmsxgdlshczskenpfwbjqoeatv
rmbxgdlihcmskgnpfwbjqoeatv
rayxgdlihczskunpfwbjqoeaef
umyxgdlisczskunpfdbjqoeatv
rmyxgdlihczskunsfwbjqieatg
rmbxgdlihczhkunpfwbjqoeamv
rmyxgdlihczskeypfwbjqxeatv
rmyxgkrihczskunptwbjqoeatv
rmyxgdlihczskunpawbjqoexiv
rmyxgdlihcrskqnpfwbjqceatv
rmyxgblihczskjnpfwbjqieatv
rmyggdlidczskunofwbjqoeatv
rmyxgdlghczskunphwbjqomatv
rmqxgdbihczskunpfnbjqoeatv
rvyxgdlihczsgunpfwbjqoeanv
royxgdlnhczskqnpfwbjqoeatv
rmyxgdlihczskugpfwbkqreatv
rmyxfdlihczskunppwejqoeatv
rqyxgdlipczskunpfwbjqoeqtv
rmyxgdlicczskunpnwbjqotatv
rmyxodlihczskxnpfwijqoeatv
rmyxrdyihczskunpftbjqoeatv
rmtxgdyihwzskunpfwbjqoeatv
tmyxcdliiczskunpfwbjqoeatv
rmyxgdlihczskmnpfwbjjoeadv
rmyxgdnihczskunpqwbjqojatv
bmyxgdlihczskcnpfwboqoeatv
rmysgdlihcyskudpfwbjqoeatv
rmyxgdtihczsmuupfwbjqoeatv
rmyxgdlihczssunpffbjqolatv
rmyogdlihczsklnpfwbjqoxatv
rmyxgjlihczskunpfwsjqoyatv
rmyxgalshczskunpfwbuqoeatv
rmyfgdlihczskunqfwbiqoeatv
tmyxgdlihczskunotwbjqoeatv
rmyxpdzihczskuopfwbjqoeatv
rmyfgdlihczskunpfrbgqoeatv
rmyxgdlwhczskhnofwbjqoeatv
rmyxgdlihczsmudpfrbjqoeatv
rmyxgdlihczokanpfwbjqooatv
rmyxrdlihczskunppwjjqoeatv
rmyxgdjihczskwnpowbjqoeatv
mmyxgdlihczikunpfwbjqoeamv
rmyxgflihczshunpwwbjqoeatv
rmytghlihczskunpfwbjqoeatk
rmyxgdlipczmbunpfwbjqoeatv
rmyxgdlihczkkonpfwbjqomatv
rmfxgslihczskunpfwujqoeatv
dmyxgdlihczykunqfwbjqoeatv
rmyxgalihcbskunpgwbjqoeatv
rmyxgdlinczqkunpfwbjqopatv
rmyxgdlihwzslunplwbjqoeatv
rmypgdlihczskdtpfwbjqoeatv
rmsxgdxieczskunpfwbjqoeatv
rmyxgdlihczskwnpfxrjqoeatv
rmyxgdlihzzskunpflbjpoeatv
rslxgdlihczsnunpfwbjqoeatv
rmyxgdlmcczskunpfwbjqoealv
fmkxgdbihczskunpfwbjqoeatv
rmyxgdiigczxkunpfwbjqoeatv
rjyxgnlqhczskunpfwbjqoeatv
ymyxgolihczskunpfmbjqoeatv
hmyxgdlihczskuncfwbjqoejtv
rmyxgqlihczzkunpfwbjqojatv
rmgfgdlihczskunpfwbjgoeatv
rmyxgdlfhczskunpfwbjqweaxv
rmoxtdlihczskunpfwdjqoeatv
ruyxgdlihczskunpfmbjnoeatv
rmnxgflehczskunpfwbjqoeatv
rmyugdlihczskunpfwfjroeatv
rmyxddbihczskunpfwbjqoeutv
rmyxgdlipczskunofbbjqoeatv
gmyxgdlihczskunpfkbjroeatv
rmyxgdllhcpskunpfwbjqqeatv
rmyxgdlihchskunpfwbjqoelcv
mmyxldlihczskuncfwbjqoeatv
ryyxgdlxhczskcnpfwbjqoeatv
rmyxpdlihczskyntfwbjqoeatv
rmhxgdlibczskwnpfwbjqoeatv
rmyxgdlihczskunpfwojbkeatv
qmyxgdlihczskunpfwbjqoyatm
rmyxgdlzhczskunpfwbjqoealr
rmyegdliqczskunpfgbjqoeatv
umyxgdlihczsvunpfwbfqoeatv
rmyxgdoihfzskunpfmbjqoeatv
rmyxgdlihcdskanpmwbjqoeatv
rmyxgdyihczskunpfrbjqoeaov
rcyxgdlihczskuegfwbjqoeatv
rmyxgdlihgwskunpfwbjkoeatv
rpyxgdlihmzskunpfwbjqoeatp
rmyxgdlihhzskunpfwbjaoeapv
rmyxgdsrhczskunpflbjqoeatv
rmrxgdlihczskunpvwbjqoeabv
rmcxgylihczskunpfwbjyoeatv
rmkxgdlyhczsounpfwbjqoeatv
rmyxgdqihczskunmfwbjqoratv
rmyxgdlihczskunpfibjqofath
rmyxgdliqczskunpqwbjqoeaev
rmhxgdlizcjskunpfwbjqoeatv
rmyxgdlfhcwskunpfwbjqoeaqv
rmyxgdlchclskunpfwbdqoeatv
rmyxgdluhczswunpfwbjqoeatt
rmyxgdlzqczskunpfwbjqoeatq
rmdxgdlihszskunpfwbwqoeatv
rmyxgdlihszsvunpfwbjqueatv
rmyxgdlhhczskunpffbjaoeatv
rmrxgdlphczskunpfwbjqreatv
hmyngdxihczskunpfwbjqoeatv
rmyxgdlizczpkunpfwbyqoeatv
rmyxbdlihyzskunlfwbjqoeatv
rmyxgdlipczsqunnfwbjqoeatv
rmyxgdlihcsskunpfxbjqoaatv
rmyxgdljhcznkunpfwbjqfeatv
rmaxgdlihczspunpfwbjqoqatv
rsyxgdlihczskunpfwbjqoehcv
rmyxgjlicczskunpfwbjqoeitv
rwymgvlihczskunpfwbjqoeatv
rmyxgdlipfzskunpfwbjqweatv
rmyxgglihczskunpgwbjqoealv
royxgdlihczskhnpfwbyqoeatv
rmyxgdlihczskvnpfabkqoeatv
rmyxgdlihczskunpfwhjwzeatv
jlyxgdlihczskunpfwbjqzeatv
rmyxgdlihccskunpfwwjqopatv
rmyxgxlihczskuupfwbjqoeahv
rmyxgdcihcbskungfwbjqoeatv
tmyxgdlihczskunpfwbjmoeftv
rkyxgdlioczskmnpfwbjqoeatv
rmyxgdlrhczskulpfwbjaoeatv
rmysgdlihczikunphwbjqoeatv
rmyxgdlihczskuvpfwbjqoeyty
fmyxgdlihczscunpfqbjqoeatv
rfyxgdlihzzrkunpfwbjqoeatv
rmyxgdlikczskunpfwbjqolath
rmyxqdlihjzskunpfwbjqoeamv
rmuxodiihczskunpfwbjqoeatv
rmyygdliucuskunpfwbjqoeatv
rmyxgdliwczskuppawbjqoeatv
rmyxgdlihczskunprwbjqgehtv
imyvgdlihczskunpfwbjqouatv
rgyxgdluhczskunpflbjqoeatv
rmgxgdlihczsdunpfwwjqoeatv
gdyxgdlihczskunpfwbjqoeavv
rmyxgdlihczskunpfwljjoektv
rmexgdlihczskunpfwxjqoeytv
rmyxqdlihcyskuwpfwbjqoeatv
rmyxgdlihczskunpfiyjqcebtv
amyngdlihczskunpfwbjqseatv
rmzxgdlihczykubpfwbjqoeatv
rmyxgdlihczhkuopfwbjsoeatv
rmyxgdlihczskunpfwbaqowztv
rmgxgdlihczslunpfwbjeoeatv
rmytgdlzhczskunrfwbjqoeatv
rmyxgdtihczskunafobjqoeatv
rmyxgdlihczskuflfbbjqoeatv
rmdxgdlihczskunpfwbjqoealj
rbyxgdlihczskuppdwbjqoeatv
rmyxhdiihcwskunpfwbjqoeatv
rmmggdlfhczskunpfwbjqoeatv
rmbxgblihczskuypfwbjqoeatv
rmyxgslihczsjunpjwbjqoeatv
rmyxgdlohczsaunpfwbjboeatv
rmaxgdhihczskunpfwbjooeatv
rmyxidlihczskunpfgbuqoeatv
rmyxgdlihfzckznpfwbjqoeatv
rmaqgdpihczskunpfwbjqoeatv
rmyvgdlirczskunpfobjqoeatv
rmdxgdlihczlkunpxwbjqoeatv
rmyxgdlihczseunpfwbjvdeatv
rmyxgdlihczskuhpfwbjqneath
rmyxrdlihciskunpfwbjqoratv
rmyxgdmihczsqunpftbjqoeatv
rmyxgdlbhczskulpfbbjqoeatv
rmoxgdlihczskunpfwbjqoeesv
rmyxgdlihczskuijfwejqoeatv
rmyxgdlihczskunpfwnkqoxatv
rmyxgdvihmzskuupfwbjqoeatv
rkyxedlihczskunpfcbjqoeatv
rmyxgdjihczskunprwbjqieatv
omyxgqgihczskunpfwbjqoeatv
rmyxydlihczskunpfwkjqoentv
rmbxgdlicczskunpfwbjqteatv
emyxgdlihczskugpfwbjqneatv
dmyxgflihczskunpfwbjqjeatv
umyxgdlihczskunpfwbjloextv
rmyxgdlihczsbunpfwbyqpeatv
rmyxgdrihczsvunpcwbjqoeatv
qmyxgdlihcwsknnpfwbjqoeatv
ymyxgdlihczskunpfsbjqowatv
rmyxgdlbhczskunpnvbjqoeatv
rmyxfdlixczskunpfwbjqoertv
rmyygdlihszrkunpfwbjqoeatv
rmyxgxlihcpskunpfwbjqoeanv
rmyxgdlihczskjnpfwbjqoprtv
rmyxgdlisczfkunpfwbjqoeath
rmyxgdlihczskunpfkbjqoeaji
rmyxgylihczskunpfwbfqoeatl
rmsxgdbihczskunpfwtjqoeatv
smyxgdlihczskunpfwbjqcwatv
rmyxgdlihczskunppjljqoeatv
rmyxgdlihczskulpfdbjooeatv
rmyxgdlihczskunpfibjqcebtv
rmyxadlihczskunpgwbjyoeatv
rmyxgdlihczdkunpvwbjqoeytv
rmyxgdlihcvskunpfwbjxohatv
rmyxgplihczskunpfgbjqoeauv
rmyxgdlihcysrunmfwbjqoeatv
rmyygdlihczskunpfwbjqvewtv
rmyxgdlihczsmunpfwdjnoeatv
rmyxgdbibczskunpfwbjuoeatv
rmyfgdlihczskubpfwbjqoeatp
rmyxgdlihczskuopfzijqoeatv
rmyqgdlihczskunpwwbjqoeanv
imyxgdlihczskunpfwbjqoqytv
rmyxgdlixcoskbnpfwbjqoeatv
rmyxgrlihccskunpfwbjqteatv
rdyxgdlihcpskunpfwbjqoratv
rmyxgdlihkzskunpfwbjmoeatj
rmyxgslihczskcnpfjbjqoeatv
rmyxgdlihczsqunqfwdjqoeatv
rjyxgdlyhczbkunpfwbjqoeatv
rmyxudlihczjkunpfwbjqzeatv
";

let%test "day2:star1:input" = checkSum(input) == 6370;
let%test "day2:star2:input" =
  commonBoxIds(input) == "rmyxgdlihczskunpfijqcebtv";