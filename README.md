# school-olympiads
Datasets and scripts for the analysis of high schooler competitions in school subjects.
The documentation in English may be forthcoming but so far we are confined to Russia (and to the Russian language as a school subject) so documentation will be provided in Russian for ease of comprehension and to avoid translation issues.

Датасеты и некоторые графики по итогам олимпиад школьников.
В настоящее время представлены данные только для России (и только для русского языка).

## Источники данных
Датасет собран исключительно на основе открытых данных, преимущественно материалов жюри регионального и заключительного этапов олимпиады и документов региональных органов власти. Сведения об источниках с комментариями можно найти в файлах [2021_Russian_sources.csv](data/2021_Russian_sources.csv) и [2022_Russian_sources.csv](data/2022_Russian_sources.csv).

## Интерпретация данных
### Интерпретация полей [2021_Russian_data.csv](data/2021_Russian_data.csv)

- `SUBJECT`: школьный предмет
- `YEAR`: год состязания
- `REGION`: регион России (включая аннексированные Республику Крым и Севастополь)
- `DATA_AVAILABLE`: имеются ли данные для региона. Данные считались имеющимися, если хотя бы для победителей и призёров регионального этапа известны результаты в баллах или в процентах от теоретического максимума, а не только статусы победителя, призёра и т. п.
- `WINNERS_ONLY`: представлены ли данные только для участников, получивших дипломы, или для всех участников регионального этапа в данном регионе. NB: для Свердловской области данные в принятом нами смысле имеются только для победителей, однако призёры включены в датасет; выбрано значение `TRUE`
- `GRADE_OF_COMPETITION`: класс, задания для которого выполнял участник (9, 10 или 11)
- `ID`: уникальный идентификатор участника (выбран произвольно)
- `CODE`: шифр участника на региональном этапе
- `DISTRICT`: район, город, муниципальное образование и т. д., к которому относится школа участника
- `SCHOOL`: школа, за которую выступает участник
- `T1`–`T8`: оценки участника в баллах за задания 1–8 регионального этапа. Несравнимы между классами!
- `SUM`: сумма баллов участника за все задания регионального этапа. Теоретическая максимально возможная сумма в 2021 году для 9 класса составляла 90 баллов, для 10 класса — 100 баллов, для 11 класса — 110 баллов; в 2022 году для 9 класса — 88 баллов, для 10 класса — 100 баллов, для 11 класса — 112 баллов
- `PERCENTAGE`: процент, который сумма баллов участника на региональном этапе составляет от теоретического максимума
- `REGIONAL_STATUS`: категория, к которой участник отнесён по итогам регионального этапа. Основными являются `Победитель`, `Призер`, `Участник`, а также пустая категория, по смыслу совпадающая со статусом `Участник`а. Отнесение следует различным правилам в разных регионах, поэтому в общем случае статусы плохо сравнимы между регионами (для отбора на заключительный этап используются не они, а единый для страны проходной балл)
- `AUTHENTIC`: какое из значений полей `SUM` и `PERCENTAGE` представлено в исходных данных, а какое вычислено при создании датасета. В случаях, когда исходным был процент выполнения (и тем более отсутствовали результаты проверки отдельных заданий), суммы несистематически приводились к разрешённому правилами шагу в 0,5 балла
- `FINAL_SUM`: сумма баллов участника на заключительном этапе. Теоретическая максимально возможная сумма в 2021 году для 9 класса составляла 150 баллов, для 10 класса — 165 баллов, для 11 класса — 180 баллов
- `FINAL_PERCENTAGE`: процент, который сумма баллов участника на заключительном этапе составляет от теоретического максимума
- `FINAL_STATUS`: категория, к которой участник отнесён по итогам заключительного этапа. Возможные значения: `Победитель`, `Призер`, `Участник`
- `COMMENT`: неформальный комментарий

### Дополнения для [2022_Russian_data.csv](data/2021_Russian_data.csv)
Условные ID для 2022 года (пока) не соотнесены с ID для 2021 года, так что их совпадение не означает совпадения личностей участников.

Региональный этап в 2022 году включал семь, а не восемь заданий, однако поле `T8` сохранено для совместимости. Оценки за задания с одинаковыми номерами в разные годы несравнимы.

### Комментарии
Данные в датасете неполны по нескольким причинам: в некоторых случаях (и во всех случаях для заключительного этапа) информация об оценках участников за отдельные задания, о районе или школе отсутствует в исходных данных; данные о школе и о шифре участника в некоторых случаях не вводились в датасет.
Кроме того, данные об участниках анонимизированы: каждому участнику сопоставлен произвольный числовой код (поле `ID`). Аналогично, из датасета исключены данные об учителях даже в тех случаях, когда в исходных данных они имеются.

В некоторых случаях исходные данные искажены (например, неверно или до апелляции подсчитаны результаты некоторых участников регионального этапа) без возможности восстановить истинную картину. Кроме того, в отдельных случаях данные отражают нарушение процедур организаторами или авторами файлов с исходными данными (например, в 2021 году все участники регионального этапа в Тамбовской области, не являющиеся победителями, охарактеризованы как `Призер`ы).

## Визуализации
Прилагаемые графики сформированы с помощью `R`. Их исходный код содержится в папке [scripts](scripts). Визуализации, для которых может быть полезен поиск, представлены в виде файлов PDF.

## Интересы
Работа выполняется автором в личном качестве и не имеет отношения к его экспертной деятельности. Организации, с которыми аффилирован автор, не имеют отношения к этой работе и не несут за неё ответственности.
