#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <ctime>
#include <cstring>
#include <cctype>
#include <sstream>
#include<bits/stdc++.h> 

using namespace std;

class Info{
	public: 
		char sNum[11];
		string name, mtrcycl, color, endTime;
		char lPlate[6];
		time_t sTime, eTime;
		struct tm*timeDate;
		char time1[30], time2[30];
		string arr[100][7];
		
		int i = 1; 
		int x; 
		int c; 
		
	int search(char sNum[]){
		x = 1;
	 	c = 0;
		for(x; x<i; x++){
			if(arr[x][0]==sNum){
				return c = 1;
			}
		}
		return c = 0;
	}
	
	bool is_studentNum(char sNum[]){
		int k;
		if(strlen(sNum) != 11) return false;
		for(k=0;k < 3; k++){
			if(!isdigit(sNum[k]))return false;
		}
		if(sNum[4] != '-')return false;
		for(k=6; k<11; k++){
			if(!isdigit(sNum[k]))return false;
		}
		return true;
	}
	
	bool is_lPlate(char lPlate[]){
		int k = 0;
		if(strlen(lPlate) != 6) return false;
		
		for(k = 0; k < 6; k++){
			if(!isalnum(lPlate[k])) return false;
		}
		return true;
	}
	
	void labels(){
		arr[0][0] = "Student Number";
		arr[0][1] = "Student Name";
		arr[0][2] = "Model";
		arr[0][3] = "License Plate";
		arr[0][4] = "Color";
		arr[0][5] = "Time of Entry";
		arr[0][6] = "Time off Exit";
	}
	
	void invalidPlate(){
		cout << "\n--------------------------------\n";
		cout << "NOTICE: The Value is Not a Motorcycle License Plate!";
		cout << "\n--------------------------------\n\n";
	}
	
	void invalidStudentNumber(){
		cout << "\n--------------------------------\n";
		cout << "NOTICE: Invalid Student Number!";
		cout << "\n--------------------------------\n\n";
	}
	
	void invalidInput(){
		cout << "\n--------------------------------\n";
		cout << "NOTICE: Invalid Input!";
		cout << "\n--------------------------------\n\n";
	}
	
	void newInfo(){
		do{
			endTime = "-";
			cout << "\n--------------------------------\n";
			cout << "\n--NEW PARKING INFORMATION--\n";
			cout << "\nPlease enter your student number: ";
			cin >> sNum;
		
			if(is_studentNum(sNum)){
				search(sNum);
			
				if(c == 0){
					cout << "Please enter the your name(First & Last Name): "; 
					getline(cin >> ws, name);
					cout << "Please enter the motorcycle's model: ";
					getline(cin >> ws, mtrcycl);
					do{
						cout << "Please enter the motorcycle's license plate/mv file number: ";
						cin >> lPlate;
						
						if(is_lPlate(lPlate) == false){
							invalidPlate();
						}
					}while(is_lPlate(lPlate) == false);
					cout << "Please enter the motorcycle's color: ";
					getline(cin >> ws, color);
					cout << "\n--------------------------------\n\n";
				}else{
					cout << "\n--------------------------------\n";
					cout << "NOTICE: The Student Number Already has an Existing Record!";
					cout << "\n--------------------------------\n\n";
				}
				
				time(&sTime);
				timeDate = localtime(&sTime);
				strftime(time1, 30, "%Y-%m-%d, %I: %M %p", timeDate);
				
				transform(name.begin(), name.end(), name.begin(), ::toupper); 
				transform(mtrcycl.begin(), mtrcycl.end(), mtrcycl.begin(), ::toupper); 
				transform(color.begin(), color.end(), color.begin(), ::toupper); 
				
				if (i < 100){
					arr[i][0] = sNum;
					arr[i][1] = name;
					arr[i][2] = mtrcycl;
					arr[i][3] = lPlate; 
					arr[i][4] = color;
					arr[i][5] = time1;
					arr[i][6] = endTime;
					i+=1;
				}else{
					cout << "\n--------------------------------\n";
					cout << "NOTICE: Database is Full!";
					cout << "\n--------------------------------\n\n";
				}
			}else{
				invalidStudentNumber();
			}
		}while(is_studentNum(sNum) == false);
	}
	
	void info(){
		cout << "--PARKING INFORMATION--";
		cout << "\n--------------------------------\n";
		cout << "STUDENT NUMBER : " << arr[x][0];
		cout << "\n--------------------------------";
		cout << "\nTIME OF ENTRY" << " : " << arr[x][5];
		cout << "\nTIME OF EXIT" << "  : " << arr[x][6];
		cout << "\n--------------------------------";
		cout << "\nSTUDENT'S NAME" << setw(18) << " : " << arr[x][1];
		cout << "\nMOTORCYCLE'S MODEL"<< setw(14) << " : " << arr[x][2];
		cout << "\nLICENSE PLATE/ MV FILE NUMBER : " << arr[x][3];
		cout << "\nMOTORCYCLE'S COLOR"<< setw(14) << " : "  << arr[x][4];
		cout << "\n--------------------------------\n";
	}
	
	void edit(){
		do{
			cout << "\n--------------------------------\n";
			cout << "\n--EDIT PARKING INFORMATION--\n";
			cout << "\nSearch Student's Number: ";
			cin >> sNum;
			cout << "\n--------------------------------\n";
			
			if(is_studentNum(sNum)){
				search(sNum);
				
				if(c == 0){
					cout << "\n--------------------------------\n";
					cout << "NOTICE: Student Number Not Found";
					cout << "\n--------------------------------\n\n";
				}else if(arr[x][6] != "-"){
					cout << "\n--------------------------------\n";
					cout << "NOTICE: Student Has Already Left the Parking";
					cout << "\n--------------------------------\n\n";
				}else{
					cout << "Please enter the your name(First & Last Name): "; 
					getline(cin >> ws, name);
					cout << "Please enter the motorcycle's model: ";
					getline(cin >> ws, mtrcycl);
					do{
						cout << "Please enter the motorcycle's license plate/mv file number: ";
						cin >> lPlate;
						if(is_lPlate(lPlate) == false){
							invalidPlate();
						}
					}while(is_lPlate(lPlate) == false);
					cout << "Please enter the motorcycle's color: ";
					getline(cin >> ws, color);
					cout << "\n--------------------------------\n\n";
					
					transform(name.begin(), name.end(), name.begin(), ::toupper); 
					transform(mtrcycl.begin(), mtrcycl.end(), mtrcycl.begin(), ::toupper); 
					transform(color.begin(), color.end(), color.begin(), ::toupper);
					
			
						arr[x][1] = name;
						arr[x][2] = mtrcycl;
						arr[x][3] = lPlate; 
						arr[x][4] = color;
					
				}
			}else{
				invalidStudentNumber();
			}
		}while(is_studentNum(sNum) == false);
		
	}
	
	void display(){
		do{
			cout << "\n--------------------------------\n";
			cout << "\n--DISPLAY PARKING INFORMATION--\n";
			cout << "\nSearch Student's Number: ";
			cin >> sNum;
			cout << "\n--------------------------------\n\n";
			
			if(is_studentNum(sNum)){
				search(sNum);
			
				if(c == 0){
					cout << "\n--------------------------------\n";
					cout << "NOTICE: Student Number Not Found";
					cout << "\n--------------------------------\n\n";
				}else{
					info();
				}
			}else{
				invalidStudentNumber();
			}
		}while(is_studentNum(sNum) == false);
	}

	void all(){
		cout << "\n--------------------------------\n";
		cout << "\n--DISPLAY ALL DATA--\n";
		cout << "\n--------------------------------\n";
		for(int a=0;a<i;a++){
			for(int b=0;b<7;b++){
				cout << arr[a][b] << "\t|" ;
			}
			cout<<"\n--------------------------------\n";
		}
		cout << "\n--------------------------------\n\n";
	}
	
	void exitTime(){
		do{
			cout << "\n--------------------------------\n";
			cout << "\n--EXIT PARKING--\n";
			cout << "\nSearch Student's Number: ";
			
			cin >> sNum;
			if(is_studentNum(sNum)){
				search(sNum);
				if(c == 0){
					cout << "\n--------------------------------\n";
					cout << "NOTICE: Student Number Not Found";
					cout << "\n--------------------------------\n\n";
				}else if(arr[x][6] != "-"){
					cout << "\n--------------------------------\n";
					cout << "NOTICE: Student Has Already Left the Parking";
					cout << "\n--------------------------------\n\n";
				}else{
					time(&eTime);
					timeDate = localtime(&eTime);
					strftime(time2, 30, "%Y-%m-%d, %I: %M %p", timeDate);
					arr[x][6] = time2;
					info();
				}
			}else{
				invalidStudentNumber();
			}
		}while(is_studentNum(sNum) == false);
	}
		
};	 

int main(){
	fstream fp;
	stringstream convert; 
	string in, privy, num, ln;
	int opt;
	Info pInfo;
	bool con = false;
	bool con1 = false;
	
	fp.open("Motor ni kwan.txt", ios::in);
	if(fp.is_open()){
		while(getline(fp, ln)){
			cout << ln<< endl;
		}
	}
	fp.close();
	
	cout<<"\n";
	
	fp.open("Data Privacy Agreement.txt", ios::in);
	if(fp.is_open()){
		while(getline(fp, ln)){
			cout << ln<< endl;
		}
	}
	while(con == false){
		cout << "\n[Y] I Agree\n";
		cout << "[N] I Do Not Agree\n";
		cout << "Do you agree: ";
		cin >> privy;
		transform(privy.begin(), privy.end(), privy.begin(), ::toupper);
		cout << "\n--------------------------------\n";
		
		if(privy == "Y"){
		pInfo.labels();		
			do{		
				cout << "\n---RTU STUDENT PARKING SYSTEM---";
				cout << "\n\nOPTIONS:";
				cout << "\n[1] New Parking";
				cout << "\n[2] Edit Parking";
				cout << "\n[3] Display Parking Information";
				cout << "\n[4] Exit Parking";
				cout << "\n[5] Display All Data";
				cout << "\n[6] Terminate Program";
				cout << "\n\nPlease select an option: ";
				cin >> in;
				if(in.size() == 1){
					convert << in;
					convert >> opt;
						switch(opt){
							case 1:
								pInfo.newInfo();	
								
								con1 = false;
								break;
							
							case 2:
								pInfo.edit(); 
									
								con1 = false;
								break;
								
							case 3:
								pInfo.display();
							
								con1  = false;
								break; 
												
							case 4:
								pInfo.exitTime();					
								
								con1 = false;
								break;
							case 5:
								pInfo.all();
								con1 = false;
								break;
							case 6:
								
								con1 = true;
								break;
								
							default:
								pInfo.invalidInput();
								con1 = false;
								break;
						}
					
				}else{
					pInfo.invalidInput();
					con1 = false;
				}
				convert.clear();	
			}while(con1 == false);
			con = true;
		
			fp.close();
			fp.open("C:\\Users\\gerem\\OneDrive\\Desktop\\GIO FILES\\Source Codes\\C++\\SYSDEV\\Database.txt", ios::out);
			if(fp.is_open()){
				for(int a=0;a<pInfo.i;a++){
					for(int b=0;b<7;b++){
						fp << pInfo.arr[a][b] << "\t|" ;
					}
					fp<<"\n--------------------------------\n";
				}
			}
			fp.close();	
			
			cout << "\n--------------------------------";
			cout << "\nTHANK YOU FOR USING THE RTU STUDENT PARKING SYSTEM!";
		}else if(privy == "N"){
			cout << "\n--------------------------------";
			cout << "\nTHANK YOU FOR USING THE RTU STUDENT PARKING SYSTEM!";
			con = true;	
		}else{
			con = false;
			pInfo.invalidInput();
		}
	}

}	
