package com.logicaldoc.dropbox;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Locale;

import com.dropbox.core.DbxException;
import com.dropbox.core.v2.files.FileMetadata;
import com.dropbox.core.v2.files.FolderMetadata;
import com.dropbox.core.v2.files.Metadata;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.security.StringEncrypter.EncryptionException;

public class DropBoxTestBench {

	public static void main(String[] args) throws IOException, URISyntaxException, DbxException, PersistenceException, EncryptionException {
		Dropbox client = new Dropbox(1L);
		boolean entered = client.login();
		System.out.println("entered " + entered);
		System.out.println(client.getAccountName());


		Metadata root = client.get("/");
		System.out.println("" + root);

		List<Metadata> entries = client.list("/");
		for (Metadata entry : entries) {
			if (entry instanceof FolderMetadata)
				System.out.println("Folder: " + entry.getName() + " " + entry.getPathDisplay());
			else
				System.out.println("File: " + entry.getName() + " " + entry.getPathDisplay());
		}

		System.out.println("\n------------\nList in Tree:\n");
		List<FileMetadata> files = client.listFilesInTree("/");
		for (FileMetadata entry : files)
			System.out.println(entry.getPathDisplay());

		File file = new File("C:\\tmp\\cmis.txt");
		client.downloadFile("/test/" + file.getName(), file);
		client.uploadFile(file, "/test/" + file.getName());
	}

	public static String authorization() throws IOException, URISyntaxException, PersistenceException, EncryptionException {
		Dropbox client = new Dropbox(1L);

		// This is for authorizing the LogicalDOC application just the first
		// time
		String authUrl = client.startAuthorization(Locale.ENGLISH);
		System.out.println("1. Go to: " + authUrl);
		System.out.println("2. Click \"Allow\" (you might have to log in first)");
		System.out.println("3. Copy the authorization code.");
		System.out.println("4. Paste the authorization code here:");

		java.awt.Desktop.getDesktop().browse(new java.net.URI(authUrl));

		String authorizationCode = new BufferedReader(new InputStreamReader(System.in)).readLine().trim();

		String accessToken = client.finishAuthorization(authorizationCode);
		System.out.println("Your access token is: " + accessToken);

		return accessToken;
	}
}