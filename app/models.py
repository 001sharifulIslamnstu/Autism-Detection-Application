from django.db import models

# Create your models here.


class DiseasesModel(models.Model):
    A1 = models.CharField(max_length=10, blank=False, null=False)
    A2 = models.CharField(max_length=10, blank=False, null=False)
    A3 = models.CharField(max_length=10, blank=False, null=False)
    A4 = models.CharField(max_length=10, blank=False, null=False)
    A5 = models.CharField(max_length=10, blank=False, null=False)
    A6 = models.CharField(max_length=10, blank=False, null=False)
    A7 = models.CharField(max_length=10, blank=False, null=False)
    A8 = models.CharField(max_length=10, blank=False, null=False)
    Age = models.FloatField(default=0, blank=False, null=False)
    Gender = models.CharField(max_length=100, blank=False, null=False)
    Ethnicity = models.CharField(max_length=100, blank=False, null=False)
    jaundice = models.CharField(max_length=10,blank=False, null=False)
    
