# Youtube Analytics
データ: https://www.kaggle.com/datasnaek/youtube-new/kernels?sortBy=hotness&group=everyone&pageSize=20&datasetId=4549&language=R

人気になる動画の傾向を調べる
[結果一覧](https://docs.google.com/document/d/1ia-fGGLPObk6rZM8Lz1jNwq97rSB719sZ_s1T7r1lxE/edit?usp=sharing)


### やろうと思ってるTODO
- タグに/descriptionに 含まれてると人気になるワードtop10
- 時期と重なるワードを使ってれば人気出る（または使っても人気でない）ex. Christmas
- てきすt分析x くらすた分析
- 時期限定12-5月
- 被ってるIDをlatestに限定 + trending durationにする


### Done
- 国ごとの人気カテゴリ
- タグが多ければviewは多いけどlikeは比較的少ない 検索に引っかかりやすくなるので -> No?
- des/title/tagの最適な長さ -> ?
- この時間帯にpublishされてれば
- 日本語ワードクラウド
- 英語ワードクラウド


### Discarded
- サムネに文字が入ってるか、色がカラフルか
- desに絵文字が含まれてるか、どれくらい含まれてるか
- タグがタイトルにこれくらい含まれてれば



### めも
人気の定義 = views?likes? views+likes? log(views)? comments? dislikes?

人気 =
- desの長さ*
- titleの長さ*
- tagの個数*
- 時期と重なるワードをいくつ使ってるか 0~(これは0/1じゃなくて個数でいいと思う。線形関係にありそうだから）
- タグがタイトルにこれくらい含まれてれば 0~
- desに絵文字が含まれてるか0/1
- この時間帯にpublishされてれば morning/evening/nightかな
- (サムネに文字が入ってるか、色がカラフルか)
- des絵文字1の中で、どれくらい含まれてるか *

人気 = views + likes - dislikes (commentsはneutralっぽいので、含まない方がいいか)
人気 = views + likes - dislikes


- 非線形なので単純な重回帰だとダメかも
重回帰分析って、非線形関係にある変数同士（desの長さは長ければ長いほどいいんじゃなくて、長すぎても短すぎてもダメ）を扱えないよね？？

