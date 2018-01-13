package com.logicaldoc.core.util;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Various utilities about users.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.1
 */
public class UserUtil {
	/**
	 * This method retrieves the user folder. If not exists, it creates the
	 * folder. The folder is: <conf.userdir>/<id>
	 * 
	 * @param id The user identifier
	 * @return
	 */
	public static File getUserHome(long id) {
		File root = getUsersDir();
		File userDir = new File(root, Long.toString(id));
		if (!userDir.exists()) {
			try {
				FileUtils.forceMkdir(userDir);
			} catch (IOException e) {
				return null;
			}
		}
		return userDir;
	}

	/**
	 * This method retrieves a user resource (file or folder). If the resource
	 * is a folder and not exists, it creates the folder. The folder will be:
	 * <conf.userdir>/<id>/<path>
	 * 
	 * @param id The user identifier
	 * @param path The resource path
	 * @return
	 */
	public static File getUserResource(long id, String path) {
		File root = getUserHome(id);
		File resource = new File(root.getPath() + "/" + path);

		if (!resource.exists() && !path.contains("."))
			try {
				FileUtils.forceMkdir(resource);
			} catch (IOException e) {
				return null;
			}

		return resource;
	}

	/**
	 * This method retrieves the users root folder. If not exists, it creates
	 * the folder. The folder is: <conf.userdir>
	 */
	public static File getUsersDir() {
		File userpath = new File("");
		try {
			ContextProperties conf = Context.get().getProperties();
			userpath = new File(conf.getPropertyWithSubstitutions("conf.userdir"));
			try {
				FileUtils.forceMkdir(userpath);
			} catch (IOException e) {
				return null;
			}
		} catch (Throwable t) {
			
		}
		return userpath;
	}
}