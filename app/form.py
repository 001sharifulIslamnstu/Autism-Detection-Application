from django import forms
from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth.models import User
from .models import *


class RegistrationForm(UserCreationForm):
    class Meta:
        model = User
        fields = ['first_name', 'username', 'password1', 'password2']


class LoginForm(forms.Form):
    class Meta:
        username = forms.CharField()
        password = forms.CharField()


class DiseasesForm(forms.ModelForm):
    class Meta:
        model = DiseasesModel
        fields = '__all__'
