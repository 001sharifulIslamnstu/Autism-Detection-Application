from django.urls import path
from . import views

urlpatterns = [
    path('register/', views.register, name="register"),
    path('', views.login_view, name="login_view"),
    path('dises/', views.dises, name="dises"),
    path('output/<str:rs>/', views.output, name="output")

]
