package com.logicaldoc.dropbox;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.dropbox.core.DbxAppInfo;
import com.dropbox.core.DbxAuthFinish;
import com.dropbox.core.DbxException;
import com.dropbox.core.DbxRequestConfig;
import com.dropbox.core.DbxWebAuth;
import com.dropbox.core.v2.DbxClientV2;
import com.dropbox.core.v2.files.FileMetadata;
import com.dropbox.core.v2.files.FolderMetadata;
import com.dropbox.core.v2.files.ListFolderErrorException;
import com.dropbox.core.v2.files.ListFolderResult;
import com.dropbox.core.v2.files.Metadata;
import com.dropbox.core.v2.files.SearchMatch;
import com.dropbox.core.v2.files.SearchResult;

/**
 * Our Dropbox facade
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class Dropbox {
	protected Logger log = LoggerFactory.getLogger(Dropbox.class);

	// Get your app key and secret from the Dropbox developers website -
	// https://www.dropbox.com/developers/apps
	final private String APP_KEY = "s7erzz9am7ikbl2";

	final private String APP_SECRET = "9shfsbeilgfizam";

	private String accessToken;

	private DbxClientV2 client;

	public boolean login(String accessToken) {
		try {
			DbxRequestConfig config = new DbxRequestConfig("LogicalDOC");
			this.client = new DbxClientV2(config, accessToken);
		} catch (Exception t) {
			log.error(t.getMessage(), t);
			return false;
		}

		// Test the connection
		try {
			return getAccountName() != null && !getAccountName().isEmpty();
		} catch (Exception t) {
			return false;
		}
	}

	private DbxWebAuth prepareWebAuth() {
		DbxAppInfo appInfo = new DbxAppInfo(APP_KEY, APP_SECRET);
		DbxRequestConfig config = new DbxRequestConfig("LogicalDOC");
		return new DbxWebAuth(config, appInfo);
	}

	/**
	 * Generates the authorization URL where the user has to allow LogicalDOC
	 * and gets an authorization code to be used then with finishAuthorization.
	 * 
	 * @param locale the user's locale 
	 * 
	 * @return The page to be shown to the user to allow the app to access
	 */
	public String startAuthorization(Locale locale) {
		DbxWebAuth webAuth = prepareWebAuth();
		DbxWebAuth.Request authRequest = DbxWebAuth.newRequestBuilder().withNoRedirect().build();
		return webAuth.authorize(authRequest);
	}

	/**
	 * Finishes the authorization process. The returned access token can be
	 * saved for future use.
	 * 
	 * @param authorizationCode The authorization code that the user see in the
	 *        authorization page

	 * @return The access token
	 */
	public String finishAuthorization(String authorizationCode) {
		DbxWebAuth webAuth = prepareWebAuth();
		try {
			DbxAuthFinish authFinish = webAuth.finishFromCode(authorizationCode);
			return authFinish.getAccessToken();
		} catch (DbxException e) {
			log.debug(e.getMessage());
		}
		return null;
	}

	public List<Metadata> list(String path) throws ListFolderErrorException, DbxException {
		String normalizedPath = path;
		if (normalizedPath.endsWith("/"))
			normalizedPath = normalizedPath.substring(0, path.length() - 1);
		if (!normalizedPath.startsWith("/"))
			normalizedPath = "/" + normalizedPath;
		if (normalizedPath.equals("/"))
			normalizedPath = "";

		ListFolderResult result = client.files().listFolder(normalizedPath);
		List<Metadata> list = result.getEntries();
		return list;
	}

	private void treeList(String parentPath, List<FileMetadata> files) throws DbxException {
		List<Metadata> list = list(parentPath);
		for (Metadata entry : list) {
			if (entry instanceof FolderMetadata)
				treeList(entry.getPathDisplay(), files);
			else
				files.add((FileMetadata) entry);
		}
	}

	public List<FileMetadata> listFilesInTree(String basePath) throws DbxException {
		List<FileMetadata> files = new ArrayList<>();
		if (basePath.endsWith("/"))
			basePath = basePath.substring(0, basePath.length() - 1);
		if (!basePath.startsWith("/"))
			basePath = "/" + basePath;
		if (basePath.equals("/"))
			basePath = "";
		treeList(basePath, files);
		return files;
	}

	public List<FileMetadata> find(String basePath, String query) throws DbxException {
		List<FileMetadata> list = new ArrayList<>();
		SearchResult result = client.files().search(basePath, query);
		List<SearchMatch> matches = result.getMatches();
		for (SearchMatch searchMatch : matches) {
			Metadata metadata = searchMatch.getMetadata();
			if (metadata instanceof FileMetadata)
				list.add((FileMetadata) metadata);
		}
		return list;
	}

	public Metadata get(String path) throws DbxException {
		if ("/".equals(path))
			return null;
		return client.files().getMetadata(path);
	}

	public boolean downloadFile(String path, File out) throws IOException {
		try(FileOutputStream stream = new FileOutputStream(out);) {
			client.files().download(path).download(stream);
			stream.flush();
			return true;
		} catch (DbxException e) {
			log.error(e.getMessage(), e);
		}
		return false;
	}

	public boolean uploadFile(File inputFile, String path) throws IOException {
		try(FileInputStream stream = new FileInputStream(inputFile);) {
			if (!path.startsWith("/"))
				path = "/" + path;
			FileMetadata uploadedFile = client.files().upload(path).uploadAndFinish(stream);
			return uploadedFile != null;
		} catch (DbxException e) {
			log.error(e.getMessage(), e);
		}
		return false;
	}

	public String getAccountName() {
		try {
			return client.users().getCurrentAccount().getName().getDisplayName();
		} catch (DbxException e) {
			return null;
		}
	}

	public String getAccessToken() {
		return accessToken;
	}
}
