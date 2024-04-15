from django.http import HttpResponse
from django.shortcuts import render, redirect
import joblib
from xgboost import XGBClassifier
from .form import *
from django.contrib.auth.decorators import login_required

from django.contrib.auth import login, logout, authenticate
import pickle




@login_required
def register(request):
    form = RegistrationForm()
    if request.method == "POST":
        form = RegistrationForm(request.POST)
        if form.is_valid():
            form.save()
            return redirect('login_view')
    return render(request, "register.html", {'form': form})

@login_required
def login_view(request):
    form = LoginForm()
    if request.method == "POST":
        form = LoginForm(request.POST)
        us = request.POST['username']
        ps = request.POST['password']
        if us is not None and ps is not None:
            user = authenticate(request, username=us, password=ps)
            if user:
                login(request, user)
                return redirect('dises')
    return render(request, 'login.html', {'form': form})


def get_output(list_data):
    model = joblib.load("app/autism_model.sav")
    prediction = model.predict([list_data])
    return prediction

@login_required
def dises(request):
    form = DiseasesForm()
    if request.method == "POST":
        form = DiseasesForm(request.POST)
        if form.is_valid():
            form.save()
            A1 = form.cleaned_data['A1']
            A2 = form.cleaned_data['A2']
            A3 = form.cleaned_data['A3']
            A4 = form.cleaned_data['A4']
            A5 = form.cleaned_data['A5']
            A6 = form.cleaned_data['A6']
            A7 = form.cleaned_data['A7']
            A8 = form.cleaned_data['A8']
            Age = form.cleaned_data['Age']
            Gender = form.cleaned_data['Gender']
            Ethnicity = form.cleaned_data['Ethnicity']
            jaundice = form.cleaned_data['jaundice']
            
            

            list_data = [Age, Gender, Ethnicity, jaundice, A1, A2, A3, A4,A5, A6, A7, A8 ]

            data = get_output(list_data)
            output = ''
            if data[0] == 0:
                output = 'result'
            else:
                output = 'results'
            return redirect('output', output)
    return render(request, "dises.html", {'form': form})


def output(request, rs):
    result = ''
    if rs == 'result':
        result = 0
    elif rs == 'results':
        result = 1

    return render(request, "output.html",{'result':result})
