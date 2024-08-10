# Android Development

::: info

The following guide explains how to set up a secondary development environment, assuming you have the relevant programming background.

:::

The Youdeyiwu Android application is primarily developed using [Flutter 3](https://flutter.dev).

You can choose your preferred development editor, though [Android Studio](https://developer.android.com/studio) is recommended.

## Clone the Repository

Clone the repository and open the project directory ```app``` with your editor.

```sh
git clone https://github.com/dafengzhen/youdeyiwu
```

## Development Configuration

**1. Install Dependencies**

Install the dependencies.

```sh
flutter pub get
```

**2. Configure the API Service**

Open the file ```lib/configs/configs.dart```.

Modify the backend request URL accordingly.

```text
const appApiServer = 'http://10.0.2.2:8080';
```

**3. Launch the Application**

Using Android Studio as an example, start an Android emulator (Device Manager) and run the application (Run 'main.dart').

## Build the APK

### Generate a Key

**1. For Windows Systems**

```USERPROFILE``` represents the directory where the key will be stored.

Run the following command to generate a key, and follow the prompts to complete the process:

```sh
keytool -genkey -v -keystore $env:USERPROFILE\upload-keystore.jks -storetype JKS -keyalg RSA -keysize 2048 -validity 10000 -alias upload
```

**2. For Other Systems or Methods**

Please refer to the [official documentation](https://docs.flutter.dev/deployment/android#create-an-upload-keystore) for guidance.

### Build the Application

**1. Run the Command**

```sh
flutter build apk
```

**2. Check the Output**

The output file can be found in the ```build/app/outputs/flutter-apk``` directory of the project.

## Other

**What if I want to build for iOS?**

As I don't have the necessary equipment, I cannot test the build process for iOS.

You might need to make some adjustments to the app for iOS compatibility.

## Conclusion

By customizing the development process, you can add various features to the application as needed.

This concludes the Android development and build process.
