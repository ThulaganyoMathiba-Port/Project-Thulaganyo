// ConsoleApplication5.cpp : This file contains the 'main' function. Program execution begins and ends there.
//This is question 4. It is very very long. It a bit confusion 

#include <iostream>
#include <string>
using namespace std;

int main() 
{
    //declare all variables!!!!!!!!!!!
    string name;
      string  surname;
      int accountNumber = 0; 
    int decision;
    double balanceOfAccount = 0.0; 
    double amount;

    bool accountCreated = false;
    //create a do loop!!!!!!!!!
    do 
    {
        cout << "\n--- Bank Account Management System ---\n";
        cout << "1. Create Account\n";
        cout << "2. Deposit Money\n";
        cout << "3. Withdraw Money\n";
        cout << "4. Check Balance\n";
        cout << "5. Display Account Details\n";
        cout << "6. Exit\n";
        cout << "Enter your choice (1-6): ";
        cin >> decision;

        switch (decision) 
        {
        case 1:
            if (accountCreated) 
            {
                cout << "Account Exists.\n";
            }
            else {
                cout << "what is your name? ";
                cin >> name;
                cout << "What is your surname? ";
                cin >> surname;
                cout << "What is your account number- Sir/Mam? ";
                cin >> accountNumber;
                cout << "What is your intial deposit? (NOTE: ensure that the amount is bigger 0) ";
                cin >> amount;

                if (amount > 0) 
                {
                    balanceOfAccount += amount;
                    accountCreated = true;
                    cout << "Account created successfully!\n";
                    cout << "New balance: R" << balanceOfAccount << endl;
                }
                else {
                    cout << "Initial deposit must be greater than 0.\n";
                }
            }
            break;

        case 2:
            if (accountCreated) 
            {
                cout << "Enter deposit amount (must be greater than 0): ";
                cin >> amount;

                if (amount > 0) {
                    balanceOfAccount += amount;
                    cout << "Deposit successful!\n";
                    cout << "New balance: R" << balanceOfAccount << endl;
                }
                else 
                {
                    cout << "Deposit amount must be greater than 0.\n";
                }
            }
            else 
            {
                cout << "Please create an account first.\n";
            }
            break;

        case 3:
            if (accountCreated)
            {
                cout << "Enter withdrawal amount: ";
                cin >> amount;

                if (amount > 0 && amount <= balanceOfAccount) {
                    balanceOfAccount -= amount;
                    cout << "Withdrawal successful!\n";
                    cout << "New balance: R" << balanceOfAccount << endl;
                }
                else if (amount > balanceOfAccount) 
                {
                    cout << "Insufficient funds.\n";
                }
                else 
                {
                    cout << "Withdrawal amount must be greater than 0.\n";
                }
            }
            else {
                cout << "Please create an account first.\n";
            }
            break;

        case 4:
            if (accountCreated) 
            {
                cout << "Current balance: R" << balanceOfAccount << endl;
            }
            else 
            {
                cout << "Please create an account first.\n";
            }
            break;

        case 5:
            if (accountCreated) 
            {
                cout << "Account Details:\n";
                cout << "Name: " << name << " " << surname << endl;
                cout << "Account Number: " << accountNumber << endl;
                cout << "Balance: R" << balanceOfAccount << endl;
            }
            else
            {
                cout << "Please create an account first.\n";
            }
            break;

        case 6:
            cout << "Have A great day. The system is exiting!\n";
            break;

        default:
            cout << "Attempt again as it the inout is ivalid!\n";
        }
    } while (decision != 6);

    return 0;
}
