package com.logicaldoc.core.security;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Models a permission, that is the ability to do something <br>
 * 
 * <ul>
 * <li>READ: permission to read</li>
 * <li>WRITE: permission to modify</li>
 * <li>ADD: permission to add child elements</li>
 * <li>SECURITY: permission to change security rules</li>
 * <li>IMMUTABILE: permission to mark a document as immutable</li>
 * <li>DELETE: permission to delete</li>
 * <li>RENAME: permission to rename</li>
 * <li>IMPORT: permission to import</li>
 * <li>EXPORT: permission to export</li>
 * <li>DOWNLOAD: permission to download</li>
 * <li>SIGN: permission to digitally sign</li>
 * <li>ARCHIVE: permission to archive</li>
 * <li>WORKFLOW: permission to handle workflow</li>
 * <li>CALENDAR: permission to handle calendar events</li>
 * <li>SUBSCRIPTION: permission to handle events subscription</li>
 * <li>PRINT: permission to print</li>
 * <li>PASSWORD: permission to put a password</li>
 * <li>MOVE: permission to move</li>
 * <li>EMAIL: permission to send emails</li>
 * <li>AUTOMATION: permission to handle the automation</li>
 * <li>STORE: permission to handle the store</li>
 * <li>READINGREQ: permission to send reading requests</li>
 * <li>PREVIEW: permission to preview a document</li>
 * <li>CUSTOMID: permission to edit the Custom ID</li>
 * <li>REVISION: permission to edit the Revision</li>
 * </ul>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public enum Permission {
	READ, DOWNLOAD, WRITE, ADD, SECURITY, IMMUTABLE, DELETE, RENAME, IMPORT, EXPORT, SIGN, ARCHIVE, WORKFLOW, CALENDAR, SUBSCRIPTION, PRINT, PASSWORD, MOVE, EMAIL, AUTOMATION, STORE, READINGREQ, PREVIEW, CUSTOMID, REVISION;

	public static Set<Permission> all() {
		return new HashSet<>(Arrays.asList(Permission.values()));
	}

	public static Set<Permission> match(String... names) {
		HashSet<Permission> set = new HashSet<>();
		for (String name : names)
			set.add(Permission.valueOf(name.toUpperCase()));
		return set;
	}

	public static Set<Permission> forGuests() {
		HashSet<Permission> set = new HashSet<>();
		set.add(READ);
		set.add(PREVIEW);
		set.add(DOWNLOAD);
		set.add(PRINT);
		set.add(EMAIL);
		set.add(SUBSCRIPTION);
		return set;
	}
}