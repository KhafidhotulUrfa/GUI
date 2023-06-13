function varargout = A20_20_Khafidhotul_Urfa_24050120120011_isi(varargin)
%A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI MATLAB code file for A20_20_Khafidhotul_Urfa_24050120120011_isi.fig
%      A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI, by itself, creates a new A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI or raises the existing
%      singleton*.
%
%      H = A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI returns the handle to a new A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI or the handle to
%      the existing singleton*.
%
%      A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI('Property','Value',...) creates a new A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI using the
%      given property value pairs. Unrecognized properties are passed via
%      varargin to A20_20_Khafidhotul_Urfa_24050120120011_isi_OpeningFcn.  This calling syntax produces a
%      warning when there is an existing singleton*.
%
%      A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI('CALLBACK') and A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI('CALLBACK',hObject,...) call the
%      local function named CALLBACK in A20_20_KHAFIDHOTUL_URFA_24050120120011_ISI.M with the given input
%      arguments.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help A20_20_Khafidhotul_Urfa_24050120120011_isi

% Last Modified by GUIDE v2.5 10-Oct-2022 19:40:08

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @A20_20_Khafidhotul_Urfa_24050120120011_isi_OpeningFcn, ...
                   'gui_OutputFcn',  @A20_20_Khafidhotul_Urfa_24050120120011_isi_OutputFcn, ...
                   'gui_LayoutFcn',  [], ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
   gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before A20_20_Khafidhotul_Urfa_24050120120011_isi is made visible.
function A20_20_Khafidhotul_Urfa_24050120120011_isi_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   unrecognized PropertyName/PropertyValue pairs from the
%            command line (see VARARGIN)

% Choose default command line output for A20_20_Khafidhotul_Urfa_24050120120011_isi
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes A20_20_Khafidhotul_Urfa_24050120120011_isi wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = A20_20_Khafidhotul_Urfa_24050120120011_isi_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
A = str2double(get(handles.edit_a,'string'));
i = str2double(get(handles.edit_i,'string'));
n = str2double(get(handles.edit_n,'string'));
m = str2double(get(handles.edit_m,'string'));
g = str2double(get(handles.edit_g,'string'));


%counting
Anuitas_biasaPV = A*(1-(1+i)^(-n))/i;
Anuitas_biasaFV= A*(((1+i)^n)-1)/i;
Anuitas_ditundaPV=(((1-((1+i)^(-n)))/i)*A)/((1+i)^(m-1));
Anuitas_ditundaFV= A*(((1+i)^n)-1)/i;
Anuitas_dimukaPV= A*(((1-(1+i)^(-n+1))/i)+1);
Anuitas_dimukaFV= ((((1+i)^n)-1)/i)*(A*(1+i));
Anuitas_bertumbuhPV= A*((1-(((1+g)/(1+i))^n))/(i-g));

question = get(handles.popupmenu1,'value');
choose1= get(handles.radiobutton1,'value');
choose2 = get(handles.radiobutton2,'value');
if question == 1 && choose1 == 1
    set(handles.edit_h,'string',Anuitas_biasaPV);
    set(handles.axes2);
    get(handles.axes2);
    x=linspace(1,n,n);
    PV=A*(1-(1+i).^(-x))/i;
    plot(x,PV,'bo-');
    title('Grafik Angsuran');
    xlabel('Periode');
    ylabel('Angsuran');

elseif question == 1 && choose2 == 1
    set(handles.edit_h,'string',Anuitas_biasaFV);
    set(handles.axes2);
    get(handles.axes2);
    x=linspace(1,n,n);
    FV= A*(((1+i).^x)-1)/i;
    plot(x,FV,'bo-');
    title('Grafik Angsuran');
    xlabel('Periode');
    ylabel('Angsuran');
    
elseif question==2 && choose1==1
    set(handles.edit_h,'string',Anuitas_ditundaPV);
    set(handles.axes2);
    get(handles.axes2);
    x=linspace(1,n,n);
    PV=(((1-((1+i).^(-x)))/i)*A)/((1+i)^(m-1));
    plot(x,PV,'bo-');
    title('Grafik Angsuran');
    xlabel('Periode');
    ylabel('Angsuran');
elseif question==2 && choose2==1
    set(handles.edit_h,'string',Anuitas_ditundaFV);
    set(handles.axes2);
    get(handles.axes2);
    x=linspace(1,n,n);
    FV= A*(((1+i).^x)-1)/i;
    plot(x,FV,'bo-');
    title('Grafik Angsuran');
    xlabel('Periode');
    ylabel('Angsuran');
elseif question==3 && choose1==1
    set(handles.edit_h,'string',Anuitas_dimukaPV);
    set(handles.axes2);
    get(handles.axes2);
    x=linspace(0,n,n);
    PV= A*(((1-(1+i).^(-x+1))/i)+1);
    plot(x,PV,'bo-');
    title('Grafik Angsuran');
    xlabel('Periode');
    ylabel('Angsuran');
elseif question==3 && choose2==1
    set(handles.edit_h,'string',Anuitas_dimukaFV);
     set(handles.axes2);
    get(handles.axes2);
    x=linspace(0,n,n);
    FV= ((((1+i).^x)-1)/i)*(A*(1+i));
    plot(x,FV,'bo-');
    title('Grafik Angsuran');
    xlabel('Periode');
    ylabel('Angsuran');
elseif question==4 && choose1==1
    set(handles.edit_h,'string',Anuitas_bertumbuhPV);
    if set(handles.radiobutton1,'enable','on');
    else set(handles.radiobutton2,'enable','off');% karena tidak ada rumus mencari nilai Future value pada anuitas bertumbuh
    end
    set(handles.axes2);
    get(handles.axes2);
    x=linspace(1,n,n);
    PV=A*((1-(((1+g)/(1+i)).^x))/(i-g));
    plot(x,PV,'bo-');
    title('Grafik Angsuran');
    xlabel('Periode');
    ylabel('Angsuran');
end





% --- Executes on button press in pushbutton2.
function pushbutton2_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
set(handles.edit_a,'string',' ');
set(handles.edit_i,'string',' ');
set(handles.edit_n,'string',' ');
set(handles.edit_m,'string',' ');
set(handles.edit_g,'string',' ');
set(handles.edit_h,'string',' ');
set(handles.radiobutton1,'enable','on');
set(handles.radiobutton2,'enable','on');
set(handles.popupmenu1,'enable','on');
x=0;
PV=0;
plot(x,PV);
title('Grafik Angsuran');
xlabel('periode');
ylabel('angsuran');
clear;
clc;


% --- Executes on button press in pushbutton3.
function pushbutton3_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
cls=questdlg('Are you sure to exit?','Close Apllication','Yes','No','default');
switch cls
case{'Yes'}
close
end



function edit_h_Callback(hObject, eventdata, handles)
% hObject    handle to edit_h (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
h=str2num(get(handles.edit_h,'string'));
handles.h=h;
guidata(hObject,handles);

% Hints: get(hObject,'String') returns contents of edit_h as text
%        str2double(get(hObject,'String')) returns contents of edit_h as a double


% --- Executes during object creation, after setting all properties.
function edit_h_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit_h (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit_a_Callback(hObject, eventdata, handles)
% hObject    handle to edit_a (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit_a as text
%        str2double(get(hObject,'String')) returns contents of edit_a as a double


% --- Executes during object creation, after setting all properties.
function edit_a_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit_a (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit_i_Callback(hObject, eventdata, handles)
% hObject    handle to edit_i (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit_i as text
%        str2double(get(hObject,'String')) returns contents of edit_i as a double


% --- Executes during object creation, after setting all properties.
function edit_i_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit_i (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit_n_Callback(hObject, eventdata, handles)
% hObject    handle to edit_n (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit_n as text
%        str2double(get(hObject,'String')) returns contents of edit_n as a double


% --- Executes during object creation, after setting all properties.
function edit_n_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit_n (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit_m_Callback(hObject, eventdata, handles)
% hObject    handle to edit_m (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit_m as text
%        str2double(get(hObject,'String')) returns contents of edit_m as a double


% --- Executes during object creation, after setting all properties.
function edit_m_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit_m (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit_g_Callback(hObject, eventdata, handles)
% hObject    handle to edit_g (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit_g as text
%        str2double(get(hObject,'String')) returns contents of edit_g as a double


% --- Executes during object creation, after setting all properties.
function edit_g_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit_g (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in radiobutton1.
function radiobutton1_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% Hint: get(hObject,'Value') returns toggle state of radiobutton1
set(handles.radiobutton2,'value',0);
rd =1;
handles.rd=rd;
guidata(hObject,handles);

% --- Executes on button press in radiobutton2.
function radiobutton2_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton2
set(handles.radiobutton1,'value',0);
rd =1;
handles.rd=rd;
guidata(hObject,handles);

% --- Executes on selection change in popupmenu1.
function popupmenu1_Callback(hObject, eventdata, handles)
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns popupmenu1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popupmenu1


% --- Executes during object creation, after setting all properties.
function popupmenu1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
