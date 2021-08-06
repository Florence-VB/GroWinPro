/*ignores peaty patents because then merged with MUP and process count data that accounts for it*/

SELECT 
tls201_appln. appln_id 
      , appln_auth + appln_nr + appln_kind 
      /*, appln_filing_date */
      , granted 
	  /*,pub_grant.publn_date as grant_date*/
      , nb_citing_docdb_fam 
FROM  tls201_appln 
  left join tls211_pat_publn pub_grant on tls201_appln.appln_id = pub_grant.appln_id and publn_first_grant ='Y'
 /* left join (select distinct appln_id, left(ipc_class_symbol,4) IPC_4 from  tls209_appln_ipc ) IPC  on tls201_appln.appln_id = IPC.appln_id*/
group by tls201_appln. appln_id 
      , appln_auth + appln_nr + appln_kind 
      , appln_filing_date 
      , granted 
	  ,pub_grant.publn_date 
      , nb_citing_docdb_fam 
      WHERE appln_auth="EP"
order by tls201_appln.appln_id;

/*gets all info from one table already available: inventors, assignees, fam size*/
            select
            appln_id,
            NB_INVENTORS,
            NB_APPLICANTS,
            DOCDB_FAMILY_SIZE,
            GRANTED,
            EARLIEST_FILING_YEAR
            from A20201_APPLN
            WHERe APPLN_AUTH='EP' AND EARLIEST_FILING_YEAR>=1992 AND EARLIEST_FILING_YEAR<=2015 ;

     
    
/*picks elements from citations. CITNORIGIN to avoid the multiple counting from applicants and examiners*/
SELECT 
*
FROM
A20212_citation;

 /*backward citations*/   
 CREATE TABLE FBS_Backwardcit_GWP AS
SELECT 
a.appln_id, a.appln_filing_date, p.publn_auth,
c.cited_pat_publn_id as bcitation,
c.NPL_CITN_SEQ_NR as nplcount
from A20201_appln a 
  join A20211_pat_publn p on p.appln_id = a.appln_id
  join A20212_citation c on  p.pat_publn_id = c.pat_publn_id
where 
/*a.APPLN_auth = 'EP' AND a.earliest_publn_year BETWEEN 2005 AND 2014*/
(a.APPLN_AUTH='EP' and c.CITN_ORIGIN='APP' AND SUBSTR(a.appln_filing_date, 1, 4)<=2015 AND SUBSTR(a.appln_filing_date, 1, 4)!= 9999) ;

/*extract the backward citations*/
select
count(distinct(bcitation)) as bkcit,
count(distinct(nplcount)) as npl,
appln_id
FROM
FBS_Backwardcit_GWP
GROUP BY appln_id ;


/*forward citations with only EP applications and within 5 years*/
/*forward citations*/
CREATE TABLE FBS_Forwardcitations_GroWinPro AS
SELECT
t1.appln_id,t2.earliest_date as corepatyear, t4.publn_date as citingdate, t3.pat_publn_id AS cites_5y
            FROM
            A20201_appln t1
            INNER JOIN
                        (SELECT
                        appln_id, MIN(publn_date) AS earliest_date
                        FROM
                        A20211_pat_publn
                        GROUP BY appln_id) 
                        t2 ON t1.appln_id = t2.appln_id
INNER JOIN
A20211_pat_publn t2b ON t2b.appln_id = t2.appln_id
INNER JOIN
A20212_citation t3 ON t2b.pat_publn_id = t3.cited_pat_publn_id
INNER JOIN
A20211_pat_publn t4 ON t3.pat_publn_id = t4.pat_publn_id
WHERE
(t1.APPLN_AUTH='EP' and t3.CITN_ORIGIN='APP' AND t1.EARLIEST_FILING_YEAR<=2015 AND SUBSTR(t4.publn_date, 1, 4)-substr(t2.earliest_date, 1, 4) <=5 AND SUBSTR(t4.publn_date, 1, 4) != 9999 AND SUBSTR(t2.earliest_date, 1, 4)!= 9999);


/*computes the citations */
select
appln_id,
count(distinct(cites_5y)) as forwardcit
from
FBS_Forwardcitations_GroWinPro
GROUP BY appln_id;

/*computes the share of X citations to measure incremental steps*/
        select 
        b.appln_ID,
        count(b.refX) as countx
        from
        
         (SELECT distinct
         t1.appln_ID,
         /*t1.pat_publn_id,*/
         w.refX
         from
         A20211_pat_publn t1
         INNER JOIN
                    (select distinct
                    PAT_PUBLN_ID as patent_citing,
                    CITN_CATEG as refX
                    FROM
                    A20215_CITN_CATEG 
                    WHERE CITN_CATEG LIKE 'x%'
                    order by CITN_CATEG) w
                    
        ON w.patent_citing=t1.pat_publn_id ) b
        group by b.appln_id;
     
                    select distinct
                    PAT_PUBLN_ID as patent_citing,
                    CITN_CATEG as refX
                    FROM
                    A20215_CITN_CATEG 
                    WHERE CITN_CATEG LIKE 'x%'
                    order by CITN_CATEG ; /*AND (t3.APPLN_AUTH='EP' AND SUBSTR(t3.appln_filing_date, 1, 4)<=2015 AND SUBSTR(t3.appln_filing_date, 1, 4)>=1980) ;
                    
                    
                    
                    
         /*counts the overall amount of ref and kicks the non alpha combinations*/
        select 
        b.appln_ID,
        count(b.reftot) as examinerref
        from
        
         (SELECT distinct
         t1.appln_ID,
         /*t1.pat_publn_id,*/
         w.reftot
         from
         A20211_pat_publn t1
         INNER JOIN
                    (select distinct
                    PAT_PUBLN_ID as patent_citing,
                    CITN_CATEG as reftot
                    FROM
                    A20215_CITN_CATEG 
                  where not regexp_like(CITN_CATEG, '^[0-9]\d+$')
                    order by CITN_CATEG) w
                    
        ON w.patent_citing=t1.pat_publn_id ) b
        group by b.appln_id;
        
                    select distinct
                    PAT_PUBLN_ID as patent_citing,
                    CITN_CATEG as refX
                    FROM
                    A20215_CITN_CATEG 
                  where not regexp_like(CITN_CATEG, '^[0-9]\d+$')
                    order by CITN_CATEG ;
     
     
    /*CPC across applnid*/ 
    CREATE TABLE FBS_GWP_CPC_list2016 as
     select
     a1.APPLN_ID,
     a1.DOCDB_FAMILY_ID,
     a1.APPLN_FILING_DATE,
     b.CPC
     FROM
     A20201_APPLN a1
      INNER JOIN
          (      
                select
                APPLN_ID,
                SUBSTR(CPC_CLASS_SYMBOL, 1, 8) as CPC
                FROM 
                A20224_APPLN_CPC) b
        
        on a1.APPLN_ID=b.APPLN_ID
        
      WHERE  a1.APPLN_AUTH='EP' AND SUBSTR(a1.appln_filing_date, 1, 4)<=2016 AND SUBSTR(a1.appln_filing_date, 1, 4)>=1990 ;
      
      /*count for each family the distinct CPC at 3 digits level*/
       CREATE TABLE FBS_GWP_CPC_scope as
      SELECT
      fq.DOCDB_FAMILY_ID,
      COUNT(fq.CPC) as scoptech
      FROM
      (select distinct
      DOCDB_FAMILY_ID,
      CPC
      from
       FBS_GWP_CPC_list2016 )  fq
       GROUP BY fq.DOCDB_FAMILY_ID ;
       
       select distinct
       DOCDB_FAMILY_ID,
       scoptech
       FROM
      FBS_GWP_CPC_scope ;
      
      
      
                /*takes the info about the tech fields*/
      
                    SELECT DISTINCT
                    tab1.APPLN_ID,
                    tab2.TECHN_FIELD_NR,
                    tab2.WEIGHT
                    FROM
                    A20230_APPLN_TECHN_FIELD tab2
                    INNER JOIN
                    A20201_APPLN tab1
                    ON tab1.APPLN_ID=tab2.APPLN_ID
                  WHERe tab1.APPLN_AUTH='EP' AND tab1.EARLIEST_FILING_YEAR>=1992 AND tab1.EARLIEST_FILING_YEAR<=2015 ;
