package scalajs.usecase.home

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global



//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching



object UnitCTT
{
  def getDemo(sDate: Int): String = {
    import java.time._
    import java.time.format.DateTimeFormatter
 
    val date0   = LocalDate.of(sDate / 10000, (sDate / 100) % 100, sDate % 100)           
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

    val sDate0 = date0.format(formatter)
    val sDate1 = date0.plusDays(1).format(formatter)
    val sDate2 = date0.plusDays(2).format(formatter)


    s"""<?xml version="1.0" encoding="utf-8"?>
    |<!DOCTYPE tournament SYSTEM 'http://www.datenautomaten.nu/dtd/nuLiga/TournamentPortal.dtd'>
    |<tournament start-date="${sDate0}" end-date="${sDate2}" name="99. Internationale Freisinger Meisterschaften"  tournament-id="FUN_TOURNAMENT">
    |     <competition age-group="Herren" start-date="${sDate0} 12:00" ttr-from="1650" ttr-remarks="S-Klasse" ttr-to="3000" type="Einzel">
    |          <players>
    |               <player id="PLAYER1" type="single">
    |                    <person birthyear="1965" club-federation-nickname="ByTTV" club-name="TV Traktor Leipzig" club-nr="111009" firstname="Ada" internal-nr="NU1000999" lastname="Kahn" licence-nr="111009155" sex="1" ttr="1792" ttr-match-count="351"/>
    |               </player>
    |               <player id="PLAYER2" type="single">
    |                    <person birthyear="1947" club-federation-nickname="ByTTV" club-name="TTC Torpedo Moskau" club-nr="105018" firstname="Addi" internal-nr="NU210649" lastname="Tion" licence-nr="105018106" sex="1" ttr="1846" ttr-match-count="118"/>
    |               </player>
    |               <player id="PLAYER3" type="single">
    |                    <person birthyear="1960" club-federation-nickname="ByTTV" club-name="TSV Rostocker Flammen" club-nr="113015" firstname="Adi" internal-nr="NU999395" lastname="Lette" licence-nr="113015005" sex="1" ttr="1922" ttr-match-count="250"/>
    |               </player>
    |               <player id="PLAYER4" type="single">
    |                    <person birthyear="1957" club-federation-nickname="ByTTV" club-name="TSV Schatzinsel" club-nr="112020" firstname="Al" internal-nr="NU1004283" lastname="ter Native" licence-nr="112020111" sex="1" ttr="1680" ttr-match-count="315"/>
    |               </player>
    |               <player id="PLAYER5" type="single">
    |                    <person birthyear="1981" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" club-nr="110011" firstname="Albert" internal-nr="NU1009099" lastname="Herum" licence-nr="110011337" sex="1" ttr="2190" ttr-match-count="77"/>
    |               </player>
    |               <player id="PLAYER6" type="single">
    |                    <person birthyear="1962" club-federation-nickname="ByTTV" club-name="SpVgg Röhrmoos-Großinzemoos" club-nr="103013" firstname="Alexander" internal-nr="NU1314710" lastname="Platz" licence-nr="103013138" sex="1" ttr="1761" ttr-match-count="123"/>
    |               </player>
    |               <player id="PLAYER7" type="single">
    |                    <person birthyear="1965" club-federation-nickname="ByTTV" club-name="FC Kiffen 08" club-nr="112002" firstname="Ali" internal-nr="NU1006294" lastname="Bert" licence-nr="112002146" sex="1" ttr="1836" ttr-match-count="392"/>
    |               </player>
    |               <player id="PLAYER8" type="single">
    |                    <person birthyear="1972" club-federation-nickname="ByTTV" club-name="VfR Netzroller" club-nr="112024" firstname="Ali" internal-nr="NU1006207" lastname="Mente" licence-nr="112024034" sex="1" ttr="1927" ttr-match-count="251"/>
    |               </player>
    |               <player id="PLAYER9" type="single">
    |                    <person birthyear="1970" club-federation-nickname="ByTTV" club-name="SV-DJK Kantenballer" club-nr="110007" firstname="Alma" internal-nr="NU1005826" lastname="Mater" licence-nr="110007242" sex="1" ttr="1669" ttr-match-count="112"/>
    |               </player>
    |               <player id="PLAYER10" type="single">
    |                    <person birthyear="1950" club-federation-nickname="ByTTV" club-name="SC Donnerfels" club-nr="106009" firstname="Anders" internal-nr="NU1011848" lastname="Gehzauch" licence-nr="106009129" sex="1" ttr="1313" ttr-match-count="153"/>
    |               </player>
    |               <player id="PLAYER11" type="single">
    |                    <person birthyear="1962" club-federation-nickname="ByTTV" club-name="TTC Scheibenwelt" club-nr="112005" firstname="Andi" internal-nr="NU1011157" lastname="Völcker-Dieserweldt" licence-nr="112005028" sex="1" ttr="1865" ttr-match-count="295"/>
    |               </player>
    |               <player id="PLAYER12" type="single">
    |                    <person birthyear="1980" club-federation-nickname="ByTTV" club-name="SV Nokturmgasse 7" club-nr="106012" firstname="Axel" internal-nr="NU1011856" lastname="Schweiß" licence-nr="106012060" sex="1" ttr="1733" ttr-match-count="118"/>
    |               </player>
    |               <player id="PLAYER13" type="single">
    |                    <person birthyear="1961" club-federation-nickname="ByTTV" club-name="SV Springfield" club-nr="112010" firstname="Kurt" internal-nr="NU1010868" lastname="Achse" licence-nr="112010153" sex="1" ttr="1996" ttr-match-count="133"/>
    |               </player>
    |               <player id="PLAYER14" type="single">
    |                    <person birthyear="1970" club-federation-nickname="ByTTV" club-name="SV Monkey Island e.V." club-nr="112027" firstname="Klaas" internal-nr="NU1260286" lastname="Container" licence-nr="112027001" sex="1" ttr="2201" ttr-match-count="67"/>
    |               </player>
    |               <player id="PLAYER15" type="single">
    |                    <person birthyear="1978" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" club-nr="110011" firstname="Klaus" internal-nr="NU1376904" lastname="Trophobie" licence-nr="110011483" sex="1" ttr="2020" ttr-match-count="12"/>
    |               </player>
    |               <player id="PLAYER16" type="single">
    |                    <person birthyear="1965" club-federation-nickname="ByTTV" club-name="TSV Rostocker Flammen" club-nr="113015" firstname="Knuth" internal-nr="NU1228839" lastname="Schenn" licence-nr="113015094" sex="1" ttr="1892" ttr-match-count="37"/>
    |               </player>
    |               <player id="PLAYER17" type="single">
    |                    <person birthyear="1995" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" club-nr="113009" firstname="Mark" internal-nr="NU1014471" lastname="Brandenburg" licence-nr="113009215" sex="1" ttr="1877" ttr-match-count="151"/>
    |               </player>
    |               <player id="PLAYER18" type="single">
    |                    <person birthyear="1988" club-federation-nickname="ByTTV" club-name="DJK SB Pandora 64" club-nr="402010" firstname="Martin" internal-nr="NU402453" lastname="Zorn" licence-nr="402010400" sex="1" ttr="1876" ttr-match-count="52"/>
    |               </player>
    |               <player id="PLAYER19" type="single">
    |                    <person birthyear="1961" club-federation-nickname="ByTTV" club-name="SG Hogsmeade" club-nr="112011" firstname="Lutz" internal-nr="NU1019633" lastname="Hose" licence-nr="112011158" sex="1" ttr="1729" ttr-match-count="450"/>
    |               </player>
    |               <player id="PLAYER20" type="single">
    |                    <person birthyear="1969" club-federation-nickname="ByTTV" club-name="SV Hundert-Morgen-Wald" club-nr="110006" firstname="Maik" internal-nr="NU1268250" lastname="Ehfer" licence-nr="110006163" sex="1" ttr="1727" ttr-match-count="91"/>
    |               </player>
    |               <player id="PLAYER21" type="single">
    |                    <person birthyear="1995" club-federation-nickname="ByTTV" club-name="SV Ihrlerstein 1958" club-nr="205015" firstname="Lukas" internal-nr="NU1017849" lastname="Hauden" licence-nr="205015165" sex="1" ttr="1731" ttr-match-count="271"/>
    |               </player>
    |               <player id="PLAYER22" type="single">
    |                    <person birthyear="1946" club-federation-nickname="ByTTV" club-name="TTC Scheibenwelt" club-nr="112005" firstname="Leif" internal-nr="NU1017876" lastname="Schaltung" licence-nr="112005044" sex="1" ttr="1863" ttr-match-count="368"/>
    |               </player>
    |               <player id="PLAYER23" type="single">
    |                    <person birthyear="1984" club-federation-nickname="ByTTV" club-name="SV Isengart" club-nr="110020" firstname="Lucky" internal-nr="NU1016452" lastname="Rung" licence-nr="110020317" sex="1" ttr="1756" ttr-match-count="214"/>
    |               </player>
    |               <player id="PLAYER24" type="single">
    |                    <person birthyear="1958" club-federation-nickname="ByTTV" club-name="TSV Lummerland e.V." club-nr="105002" firstname="Niko" internal-nr="NU1128537" lastname="Thien" licence-nr="105002220" sex="1" ttr="1957" ttr-match-count="221"/>
    |               </player>
    |               <player id="PLAYER25" type="single">
    |                    <person birthyear="1995" club-federation-nickname="ByTTV" club-name="TSV Lummerland e.V." club-nr="105002" firstname="Nat" internal-nr="NU1137488" lastname="Zwerg" licence-nr="105002221" sex="1" ttr="1937" ttr-match-count="16"/>
    |               </player>
    |               <player id="PLAYER26" type="single">
    |                    <person birthyear="1961" club-federation-nickname="ByTTV" club-name="TSV 1864 Twin Peaks" club-nr="113006" firstname="Nick" internal-nr="NU1021171" lastname="Elmine" licence-nr="113006076" sex="1" ttr="1831" ttr-match-count="245"/>
    |               </player>
    |               <player id="PLAYER27" type="single">
    |                    <person birthyear="1960" club-federation-nickname="ByTTV" club-name="TTC Orgrimmar" club-nr="112023" firstname="Till" internal-nr="NU1022538" lastname="Sitter" licence-nr="112023004" sex="1" ttr="1792" ttr-match-count="356"/>
    |               </player>
    |               <player id="PLAYER28" type="single">
    |                    <person birthyear="1944" club-federation-nickname="ByTTV" club-name="TSV Rostocker Flammen" club-nr="113015" firstname="Thorn" internal-nr="NU1020762" lastname="Ister" licence-nr="113015101" sex="1" ttr="1932" ttr-match-count="47"/>
    |               </player>
    |               <player id="PLAYER29" type="single">
    |                    <person birthyear="1972" club-federation-nickname="ByTTV" club-name="DJK SB Pandora 64" club-nr="402010" firstname="Theo" internal-nr="NU1334904" lastname="Retisch" licence-nr="402010406" sex="1" ttr="2290" ttr-match-count="19"/>
    |               </player>
    |               <player id="PLAYER30" type="single">
    |                    <person birthyear="1967" club-federation-nickname="ByTTV" club-name="TSV 1907 Indersdorf" club-nr="103008" firstname="Tom" internal-nr="NU1025642" lastname="Ate" licence-nr="103008070" sex="1" ttr="1802" ttr-match-count="184"/>
    |               </player>
    |               <player id="PLAYER31" type="single">
    |                    <person birthyear="1967" club-federation-nickname="ByTTV" club-name="FC Askaban" club-nr="112025" firstname="Wilhelm" internal-nr="NU1129013" lastname="Zafen" licence-nr="112025021" sex="1" ttr="1913" ttr-match-count="134"/>
    |               </player>
    |               <player id="PLAYER32" type="single">
    |                    <person birthyear="1973" club-federation-nickname="ByTTV" club-name="SV Sarching" club-nr="401009" firstname="Sam" internal-nr="NU1023346" lastname="Elb-Rösel" licence-nr="401009068" sex="1" ttr="1874" ttr-match-count="166"/>
    |               </player>
    |               <player id="PLAYER33" type="single">
    |                    <person birthyear="1994" club-federation-nickname="ByTTV" club-name="TTC Phatt Island" club-nr="110018" firstname="Sepp A." internal-nr="NU1023592" lastname="Rator" licence-nr="110018285" sex="1" ttr="1847" ttr-match-count="429"/>
    |               </player>
    |               <player id="PLAYER34" type="single">
    |                    <person birthyear="1964" club-federation-nickname="ByTTV" club-name="ESV Wonderland" club-nr="110010" firstname="Ron" internal-nr="NU1023635" lastname="ter Holen" licence-nr="110010102" sex="1" ttr="1956" ttr-match-count="38"/>
    |               </player>
    |               <player id="PLAYER35" type="single">
    |                    <person birthyear="1976" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" club-nr="110011" firstname="Roman" internal-nr="NU1184966" lastname="Held" licence-nr="110011468" sex="1" ttr="1883" ttr-match-count="76"/>
    |               </player>
    |               <player id="PLAYER36" type="single">
    |                    <person birthyear="1988" club-federation-nickname="ByTTV" club-name="TSV Posemuckl 1862" club-nr="112004" firstname="Stig" internal-nr="NU1029682" lastname="Matisierung" licence-nr="112004159" sex="1" ttr="1790" ttr-match-count="392"/>
    |               </player>
    |               <player id="PLAYER37" type="single">
    |                    <person birthyear="1990" club-federation-nickname="ByTTV" club-name="TTC Orgrimmar" club-nr="112023" firstname="Qu" internal-nr="NU1029426" lastname="Chen" licence-nr="112623062" sex="1" ttr="1878" ttr-match-count="350"/>
    |               </player>
    |               <player id="PLAYER38" type="single">
    |                    <person birthyear="1955" club-federation-nickname="ByTTV" club-name="TSV Schatzinsel" club-nr="112020" firstname="Rainer" internal-nr="NU1031921" lastname="Zufall" licence-nr="112020089" sex="1" ttr="1765" ttr-match-count="353"/>
    |               </player>
    |               <player id="PLAYER39" type="single">
    |                    <person birthyear="1976" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" club-nr="110011" firstname="Roy" internal-nr="NU1229922" lastname="Bär" licence-nr="110411432" sex="1" ttr="1661" ttr-match-count="76"/>
    |               </player>
    |               <player id="PLAYER40" type="single">
    |                    <person birthyear="1992" club-federation-nickname="ByTTV" club-name="TV Quahog" club-nr="202004" firstname="David" internal-nr="NU1136164" lastname="Getter" licence-nr="202004161" sex="1" ttr="1531" ttr-match-count="54"/>
    |               </player>
    |               <player id="PLAYER41" type="single">
    |                    <person birthyear="1999" club-federation-nickname="ByTTV" club-name="TV Quahog" club-nr="202004" firstname="Ross" internal-nr="NU1281105" lastname="Haar" licence-nr="202004168" sex="1" ttr="1904" ttr-match-count="82"/>
    |               </player>
    |               <player id="PLAYER42" type="single">
    |                    <person birthyear="1987" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" club-nr="113009" firstname="Ryan" internal-nr="NU1034057" lastname="Kommen" licence-nr="113009184" sex="1" ttr="1904" ttr-match-count="181"/>
    |               </player>
    |               <player id="PLAYER43" type="single">
    |                    <person birthyear="1969" club-federation-nickname="ByTTV" club-name="TTC Orgrimmar" club-nr="112023" firstname="Sam" internal-nr="NU1200096" lastname="Pler" licence-nr="112023107" sex="1" ttr="1752" ttr-match-count="58"/>
    |               </player>
    |               <player id="PLAYER44" type="single">
    |                    <person birthyear="1958" club-federation-nickname="ByTTV" club-name="SC Xanadu" club-nr="104010" firstname="Stefan S." internal-nr="NU1039231" lastname="Dom" licence-nr="104010110" sex="1" ttr="1801" ttr-match-count="142"/>
    |               </player>
    |               <player id="PLAYER45" type="single">
    |                    <person birthyear="1956" club-federation-nickname="ByTTV" club-name="TSV Lummerland e.V." club-nr="105002" firstname="Ted" internal-nr="NU1231950" lastname="O'Wierung" licence-nr="105002194" sex="1" ttr="2019" ttr-match-count="81"/>
    |               </player>
    |               <player id="PLAYER46" type="single">
    |                    <person birthyear="1985" club-federation-nickname="ByTTV" club-name="TTC Zaremonien" club-nr="103022" firstname="Phil" internal-nr="NU1042740" lastname="Harmonie" licence-nr="103022295" sex="1" ttr="1711" ttr-match-count="242"/>
    |               </player>
    |               <player id="PLAYER47" type="single">
    |                    <person birthyear="1984" club-federation-nickname="ByTTV" club-name="SV Blood Island" club-nr="109014" firstname="Otto" internal-nr="NU1043423" lastname="Motor" licence-nr="109014130" sex="1" ttr="1778" ttr-match-count="187"/>
    |               </player>
    |               <player id="PLAYER48" type="single">
    |                    <person birthyear="1975" club-federation-nickname="ByTTV" club-name="Post SV Avalon" club-nr="102006" firstname="Peer" internal-nr="NU1052396" lastname="Söhnlich" licence-nr="102006122" sex="1" ttr="1771" ttr-match-count="187"/>
    |               </player>
    |               <player id="PLAYER49" type="single">
    |                    <person birthyear="1965" club-federation-nickname="ByTTV" club-name="SC Dinky Island" club-nr="105008" firstname="Nils" internal-nr="NU1258460" lastname="Pferd" licence-nr="105008044" sex="1" ttr="1851" ttr-match-count="106"/>
    |               </player>
    |               <player id="PLAYER50" type="single">
    |                    <person birthyear="1961" club-federation-nickname="ByTTV" club-name="SC Narnia" club-nr="106017" firstname="Pedro" internal-nr="NU1056499" lastname="Leum" licence-nr="106017121" sex="1" ttr="1962" ttr-match-count="465"/>
    |               </player>
    |               <player id="PLAYER51" type="single">
    |                    <person birthyear="1976" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" club-nr="110011" firstname="Otto" internal-nr="NU1055357" lastname="Päde" licence-nr="110011433" sex="1" ttr="1657" ttr-match-count="37"/>
    |               </player>
    |               <player id="PLAYER52" type="single">
    |                    <person birthyear="1977" club-federation-nickname="ByTTV" club-name="TTC Zaremonien" club-nr="103022" firstname="Mirko" internal-nr="NU1060735" lastname="Welle" licence-nr="103022128" sex="1" ttr="1733" ttr-match-count="223"/>
    |               </player>
    |               <player id="PLAYER53" type="single">
    |                    <person birthyear="1992" club-federation-nickname="ByTTV" club-name="SV PanneKlopper" club-nr="205003" firstname="Mike" internal-nr="NU1060146" lastname="Rofon" licence-nr="205003172" sex="1" ttr="1793" ttr-match-count="200"/>
    |               </player>
    |               <player id="PLAYER54" type="single">
    |                    <person birthyear="1992" club-federation-nickname="ByTTV" club-name="TSV 1880 Reichenstadt" club-nr="106013" firstname="Matt" internal-nr="NU1311290" lastname="Jess" licence-nr="106013324" sex="1" ttr="1975" ttr-match-count="17"/>
    |               </player>
    |               <player id="PLAYER55" type="single">
    |                    <person birthyear="1978" club-federation-nickname="ByTTV" club-name="TTC Torpedo Moskau" club-nr="105018" firstname="Max" internal-nr="NU1175847" lastname="Strammer" licence-nr="105018109" sex="1" ttr="1784" ttr-match-count="43"/>
    |               </player>
    |          </players>
    |     </competition>
    |     <competition age-group="Damen" start-date="${sDate1} 08:00" ttr-from="0" ttr-remarks="D-Klasse" ttr-to="1200" type="Einzel">
    |          <players>
    |               <player id="PLAYER56" type="single">
    |                    <person birthyear="1976" club-federation-nickname="ByTTV" club-name="MBB SG Motzen" club-nr="102011" firstname="Lene" internal-nr="NU998576" lastname="Arm" licence-nr="102011102" sex="1" ttr="1195" ttr-match-count="155"/>
    |               </player>
    |               <player id="PLAYER57" type="single">
    |                    <person birthyear="1962" club-federation-nickname="ByTTV" club-name="TTC Phatt Island" club-nr="110018" firstname="Lotte" internal-nr="NU1279403" lastname="Macchiato" licence-nr="110018353" sex="1" ttr="1000" ttr-match-count="82"/>
    |               </player>
    |               <player id="PLAYER58" type="single">
    |                    <person birthyear="1991" club-federation-nickname="ByTTV" club-name="TSV Jux" club-nr="106003" firstname="Liz" internal-nr="NU1127543" lastname="Ten Platz" licence-nr="106003307" sex="1" ttr="1020" ttr-match-count="260"/>
    |               </player>
    |               <player id="PLAYER59" type="single">
    |                    <person birthyear="1984" club-federation-nickname="ByTTV" club-name="1. SC Haarzopf" club-nr="104008" firstname="Luzie" internal-nr="NU999584" lastname="Fehr" licence-nr="104008192" sex="1" ttr="1172" ttr-match-count="264"/>
    |               </player>
    |               <player id="PLAYER60" type="single">
    |                    <person birthyear="1980" club-federation-nickname="ByTTV" club-name="SC Narnia" club-nr="106017" firstname="Marga" internal-nr="NU1005416" lastname="Quark" licence-nr="106017260" sex="1" ttr="954" ttr-match-count="348"/>
    |               </player>
    |               <player id="PLAYER61" type="single">
    |                    <person birthyear="1995" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" club-nr="113009" firstname="Rita" internal-nr="NU1004059" lastname="Sport" licence-nr="113009212" sex="1" ttr="873" ttr-match-count="119"/>
    |               </player>
    |               <player id="PLAYER62" type="single">
    |                    <person birthyear="1984" club-federation-nickname="ByTTV" club-name="SV Buntekuh" club-nr="116024" firstname="Pia" internal-nr="NU1002499" lastname="Nist" licence-nr="116024054" sex="1" ttr="1111" ttr-match-count="229"/>
    |               </player>
    |               <player id="PLAYER63" type="single">
    |                    <person birthyear="1966" club-federation-nickname="ByTTV" club-name="SV Champhausen" club-nr="105010" firstname="Polly" internal-nr="NU1008415" lastname="Zeih" licence-nr="105010060" sex="1" ttr="1146" ttr-match-count="315"/>
    |               </player>
    |               <player id="PLAYER64" type="single">
    |                    <person birthyear="1996" club-federation-nickname="ByTTV" club-name="TV Entenhausen" club-nr="109012" firstname="Ingrid" internal-nr="NU1009163" lastname="Enzien" licence-nr="109012113" sex="1" ttr="1003" ttr-match-count="311"/>
    |               </player>
    |               <player id="PLAYER65" type="single">
    |                    <person birthyear="1995" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" club-nr="113009" firstname="Joyce" internal-nr="NU1008691" lastname="Tick" licence-nr="113009214" sex="1" ttr="935" ttr-match-count="166"/>
    |               </player>
    |               <player id="PLAYER66" type="single">
    |                    <person birthyear="1995" club-federation-nickname="ByTTV" club-name="TSV Lederhose" club-nr="102010" firstname="Caro" internal-nr="NU1014014" lastname="Ass" licence-nr="102010067" sex="1" ttr="826" ttr-match-count="174"/>
    |               </player>
    |               <player id="PLAYER67" type="single">
    |                    <person birthyear="1995" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" club-nr="113009" firstname="Cara" internal-nr="NU1014471" lastname="Biener-Haken" licence-nr="113009215" sex="1" ttr="1177" ttr-match-count="151"/>
    |               </player>
    |               <player id="PLAYER68" type="single">
    |                    <person birthyear="1961" club-federation-nickname="ByTTV" club-name="SG Hogsmeade" club-nr="112011" firstname="Christa" internal-nr="NU1019633" lastname="Leuchter" licence-nr="112011158" sex="1" ttr="1129" ttr-match-count="450"/>
    |               </player>
    |               <player id="PLAYER69" type="single">
    |                    <person birthyear="1986" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" club-nr="113009" firstname="Claudia" internal-nr="NU1177338" lastname="Manten" licence-nr="113009233" sex="1" ttr="1009" ttr-match-count="53"/>
    |               </player>
    |               <player id="PLAYER70" type="single">
    |                    <person birthyear="1973" club-federation-nickname="ByTTV" club-name="SV Feucht" club-nr="103029" firstname="Claire" internal-nr="NU1022365" lastname="Grube" licence-nr="103029171" sex="1" ttr="1331" ttr-match-count="252"/>
    |               </player>
    |               <player id="PLAYER71" type="single">
    |                    <person birthyear="1967" club-federation-nickname="ByTTV" club-name="DJK SV Mückenloch" club-nr="202005" firstname="Conny" internal-nr="NU1021958" lastname="Fähre" licence-nr="202005099" sex="1" ttr="1169" ttr-match-count="214"/>
    |               </player>
    |               <player id="PLAYER72" type="single">
    |                    <person birthyear="1957" club-federation-nickname="ByTTV" club-name="FT Vogelsang 1966" club-nr="105005" firstname="Cora" internal-nr="NU1020649" lastname="Schiert" licence-nr="105005337" sex="1" ttr="1084" ttr-match-count="314"/>
    |               </player>
    |               <player id="PLAYER73" type="single">
    |                    <person birthyear="1982" club-federation-nickname="ByTTV" club-name="TTC Garching" club-nr="112006" firstname="Thorsten" internal-nr="NU239198" lastname="Kaemmerling" licence-nr="112006264" sex="1" ttr="917" ttr-match-count="225"/>
    |               </player>
    |               <player id="PLAYER74" type="single">
    |                    <person birthyear="1995" club-federation-nickname="ByTTV" club-name="TSV Posemuckl 1862" club-nr="112004" firstname="Donna" internal-nr="NU1025584" lastname="Littchen" licence-nr="112004206" sex="1" ttr="933" ttr-match-count="397"/>
    |               </player>
    |               <player id="PLAYER75" type="single">
    |                    <person birthyear="1965" club-federation-nickname="ByTTV" club-name="TSV Altenheim" club-nr="111012" firstname="Anka" internal-nr="NU1026037" lastname="Kette" licence-nr="111012015" sex="1" ttr="1085" ttr-match-count="397"/>
    |               </player>
    |               <player id="PLAYER76" type="single">
    |                    <person birthyear="1992" club-federation-nickname="ByTTV" club-name="TSV Greding" club-nr="608004" firstname="Martin" internal-nr="NU1024310" lastname="Kern jun." licence-nr="608004133" sex="1" ttr="1046" ttr-match-count="268"/>
    |               </player>
    |               <player id="PLAYER77" type="single">
    |                    <person birthyear="1985" club-federation-nickname="ByTTV" club-name="1. FC Katzenhirn" club-nr="608014" firstname="Ann" internal-nr="NU1023481" lastname="Probe" licence-nr="608014266" sex="1" ttr="1145" ttr-match-count="318"/>
    |               </player>
    |               <player id="PLAYER78" type="single">
    |                    <person birthyear="1962" club-federation-nickname="ByTTV" club-name="TTC Kuhbier" club-nr="109001" firstname="Anna" internal-nr="NU1045465" lastname="Nass" licence-nr="109001045" sex="1" ttr="1130" ttr-match-count="239"/>
    |               </player>
    |               <player id="PLAYER79" type="single">
    |                    <person birthyear="1973" club-federation-nickname="ByTTV" club-name="TSV Schlägerholz" club-nr="110815" firstname="Bart" internal-nr="NU1048485" lastname="Stoppel" licence-nr="110015224" sex="1" ttr="1183" ttr-match-count="209"/>
    |               </player>
    |               <player id="PLAYER80" type="single">
    |                    <person birthyear="1986" club-federation-nickname="ByTTV" club-name="MBB SG Motzen" club-nr="102011" firstname="Bea" internal-nr="NU1053373" lastname="Lldich" licence-nr="102011076" sex="1" ttr="1112" ttr-match-count="255"/>
    |               </player>
    |          </players>
    |     </competition>
    |     <competition age-group="Damen" start-date="${sDate1} 08:00" ttr-from="1501" ttr-remarks="B-Klasse" ttr-to="1650" type="Einzel">
    |          <players>
    |               <player id="PLAYER81" type="single">
    |                    <person birthyear="1992" club-federation-nickname="ByTTV" club-name="TSV Hundeluft 65" club-nr="103006" firstname="Donna" internal-nr="NU995873" lastname="Wetter" licence-nr="103006228" sex="1" ttr="1547" ttr-match-count="244"/>
    |               </player>
    |               <player id="PLAYER82" type="single">
    |                    <person birthyear="1963" club-federation-nickname="ByTTV" club-name="VfL Busenberg e.V." club-nr="113013" firstname="Ella" internal-nr="NU996465" lastname="Nut" licence-nr="113013283" sex="1" ttr="1547" ttr-match-count="275"/>
    |               </player>
    |               <player id="PLAYER83" type="single">
    |                    <person birthyear="1995" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" club-nr="110011" firstname="Esther" internal-nr="NU997673" lastname="Poly" licence-nr="110011360" sex="1" ttr="1535" ttr-match-count="258"/>
    |               </player>
    |               <player id="PLAYER84" type="single">
    |                    <person birthyear="1999" club-federation-nickname="ByTTV" club-name="TSV Sommerloch" club-nr="111010" firstname="Hanni" internal-nr="NU205625" lastname="Muhn" licence-nr="111010235" sex="1" ttr="1649" ttr-match-count="326"/>
    |               </player>
    |               <player id="PLAYER85" type="single">
    |                    <person birthyear="1981" club-federation-nickname="ByTTV" club-name="TTC Scheibenwelt" club-nr="112005" firstname="Grete" internal-nr="NU998085" lastname="Fisch" licence-nr="112005008" sex="1" ttr="1618" ttr-match-count="238"/>
    |               </player>
    |               <player id="PLAYER86" type="single">
    |                    <person birthyear="1989" club-federation-nickname="ByTTV" club-name="SC Pandora 64 e.V." club-nr="402015" firstname="Insa" internal-nr="NU1001065" lastname="Ihr" licence-nr="402015203" sex="1" ttr="1534" ttr-match-count="384"/>
    |               </player>
    |               <player id="PLAYER87" type="single">
    |                    <person birthyear="1964" club-federation-nickname="ByTTV" club-name="TV Entenhausen" club-nr="109012" firstname="Malte" internal-nr="NU1009164" lastname="Sehr" licence-nr="109012147" sex="1" ttr="1616" ttr-match-count="290"/>
    |               </player>
    |               <player id="PLAYER88" type="single">
    |                    <person birthyear="1978" club-federation-nickname="ByTTV" club-name="TSV Gammelshausen e.V." club-nr="103009" firstname="Mary" internal-nr="NU1008382" lastname="Krissmes" licence-nr="103009274" sex="1" ttr="1454" ttr-match-count="199"/>
    |               </player>
    |               <player id="PLAYER89" type="single">
    |                    <person birthyear="1971" club-federation-nickname="ByTTV" club-name="TSV Gammelshausen e.V." club-nr="103009" firstname="Maria" internal-nr="NU1008382" lastname="Nengraben" licence-nr="103809274" sex="1" ttr="1354" ttr-match-count="199"/>
    |               </player>
    |               <player id="PLAYER90" type="single">
    |                    <person birthyear="1976" club-federation-nickname="ByTTV" club-name="MBB SG Motzen" club-nr="102011" firstname="Lene" internal-nr="NU998576" lastname="Arm" licence-nr="102011102" sex="1" ttr="1195" ttr-match-count="155"/>
    |               </player>
    |               <player id="PLAYER91" type="single">
    |                    <person birthyear="1986" club-federation-nickname="ByTTV" club-name="MBB SG Motzen" club-nr="102011" firstname="Bea" internal-nr="NU1053373" lastname="Lldich" licence-nr="102011076" sex="1" ttr="1112" ttr-match-count="255"/>
    |               </player>
    |          </players>
    |     </competition>
    |     <competition age-group="Herren" start-date="${sDate2} 14:00" ttr-from="1650" ttr-remarks="S-Klasse" ttr-to="3000" type="Doppel">
    |          <players>
    |               <player id="PLAYER92" type="double">
    |                    <person birthyear="1965" club-federation-nickname="ByTTV" club-name="TV Traktor Leipzig" club-nr="111009" firstname="Ada" internal-nr="NU1000999" lastname="Kahn" licence-nr="111009155" sex="1" ttr="1792" ttr-match-count="351"/>
    |                    <person birthyear="1947" club-federation-nickname="ByTTV" club-name="TTC Torpedo Moskau" club-nr="105018" firstname="Addi" internal-nr="NU210649" lastname="Tion" licence-nr="105018106" sex="1" ttr="1846" ttr-match-count="118"/>
    |               </player>
    |               <player id="PLAYER93" type="double">
    |                    <person birthyear="1960" club-federation-nickname="ByTTV" club-name="TSV Rostocker Flammen" club-nr="113015" firstname="Adi" internal-nr="NU999395" lastname="Lette" licence-nr="113015005" sex="1" ttr="1922" ttr-match-count="250"/>
    |                    <person birthyear="1957" club-federation-nickname="ByTTV" club-name="TSV Schatzinsel" club-nr="112020" firstname="Al" internal-nr="NU1004283" lastname="ter Native" licence-nr="112020111" sex="1" ttr="1680" ttr-match-count="315"/>
    |               </player>
    |               <player id="PLAYER94" type="double">
    |                    <person birthyear="1981" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" club-nr="110011" firstname="Albert" internal-nr="NU1009099" lastname="Herum" licence-nr="110011337" sex="1" ttr="2190" ttr-match-count="77"/>
    |                    <person birthyear="1962" club-federation-nickname="ByTTV" club-name="SpVgg Röhrmoos-Großinzemoos" club-nr="103013" firstname="Alexander" internal-nr="NU1314710" lastname="Platz" licence-nr="103013138" sex="1" ttr="1761" ttr-match-count="123"/>
    |               </player>
    |               <player id="PLAYER95" type="double">
    |                    <person birthyear="1965" club-federation-nickname="ByTTV" club-name="FC Kiffen 08" club-nr="112002" firstname="Ali" internal-nr="NU1006294" lastname="Bert" licence-nr="112002146" sex="1" ttr="1836" ttr-match-count="392"/>
    |                    <person birthyear="1972" club-federation-nickname="ByTTV" club-name="VfR Netzroller" club-nr="112024" firstname="Ali" internal-nr="NU1006207" lastname="Mente" licence-nr="112024034" sex="1" ttr="1927" ttr-match-count="251"/>
    |               </player>
    |          </players>
    |     </competition>
    |</tournament>
    """.stripMargin('|')
  } 
}