When building under OpenVMS, you can use a slightly modified formatting, which I have done here.  It is the same code as the parent directory, but it stops the compiler complaining.

Note that OpenVMS uses the underlying RMS system to manage file records, so you need to create a suitable data file after you compile the example.  To do this, issue the following command:

CREATE/FDL=ACCOUNT.FDL ACCTFILE.DAT

No special permissions are required.
