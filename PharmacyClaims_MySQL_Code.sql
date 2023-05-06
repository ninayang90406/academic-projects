# Set primary keys for each table
alter table dim_drug
add primary key (drug_ndc);
alter table dim_drug_brand_generic
add primary key (drug_brand_generic_code);
alter table dim_drug_form
add primary key (drug_form_code);
alter table dim_member
add primary key (member_id);
alter table fact_fill
add id int not null auto_increment primary key;

# Set foreign keys for each table-SET NULL
alter table fact_fill
add foreign key fact_fill_member_id_fk(member_id)
references dim_member(member_id)
on delete set null
on update set null;
alter table fact_fill
add foreign key fact_fill_drug_ndc_fk(drug_ndc)
references dim_drug(drug_ndc)
on delete set null
on update set null;
alter table dim_drug
add foreign key dim_drug_drug_brand_generic_code_fk(drug_brand_generic_code)
references dim_drug_brand_generic(drug_brand_generic_code)
on delete set null
on update set null;
alter table dim_drug
add foreign key dim_drug_drug_form_code_fk(drug_form_code)
references dim_drug_form(drug_form_code)
on delete set null
on update set null;

# Part 4
# Question 1
select d.drug_name, count(f.member_id) as number_prescriptions
from dim_drug d inner join fact_fill f
on d.drug_ndc = f.drug_ndc
group by drug_name;

# Question 2
select case 
when d.member_age > 65 then 'age 65+'
when d.member_age < 65 then '< 65'
end as age_group,
count(distinct d.member_id) as number_members, sum(f.copay) as sum_copay, sum(f.insurancepaid) as sum_insurancepaid,
count(f.member_id) as number_prescriptions
from dim_member d inner join fact_fill f
on d.member_id = f.member_id
group by age_group;

# Question 3
#some joins to be ready for partition
create table presp_fill as
select d.member_id, d.member_first_name, d.member_last_name, dr.drug_name,
str_to_date(f.fill_date, '%m/%d/%Y') as fill_date_mr, f.insurancepaid 
from dim_member d inner join fact_fill f on d.member_id = f.member_id
inner join dim_drug dr on dr.drug_ndc = f.drug_ndc;

#window function to generate number of fills
select * from presp_fill;
create table insurancepaid_info as
select member_id, member_first_name, member_last_name, drug_name, fill_date_mr, insurancepaid, 
row_number() over (partition by member_id order by member_id,fill_date_mr desc) as fill_times
from presp_fill;

# only take the each member's most recent fill
select * from insurancepaid_info
where fill_times = 1; 