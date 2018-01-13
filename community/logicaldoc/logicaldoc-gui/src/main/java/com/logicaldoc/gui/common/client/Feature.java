package com.logicaldoc.gui.common.client;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.gui.common.client.beans.GUIInfo;

/**
 * Stores the enabled features
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class Feature {

	public static final int IMPORT_REMOTE_FOLDERS = 1;

	public static final int ADDITIONAL_FORMATS = 2;

	public static final int OCR = 3;

	public static final int IMPEX = 4;

	public static final int TAGS = 5;

	public static final int BRANDING_STANDARD = 6;

	public static final int DIGITAL_SIGNATURE = 7;

	public static final int PDF = 8;

	public static final int RSS = 9;

	public static final int EMAIL_IMPORT = 10;

	public static final int TRASH = 11;

	public static final int SAVED_SEARCHES = 12;

	public static final int MESSAGES = 13;

	public static final int INCREMENTAL_ARCHIVES = 14;

	public static final int AOS = 15;

	public static final int SCAN = 16;

	public static final int AUDIT = 17;

	public static final int NOTIFICATIONS = 18;

	public static final int WORKFLOW = 19;

	public static final int OFFICE = 20;

	public static final int ENTERPRISE_SEARCH = 21;

	public static final int NOTES = 22;

	public static final int TAGS_ADMIN = 23;

	public static final int GUI_LANGUAGES = 24;

	public static final int INDEX_LANGUAGES = 25;

	public static final int TASK_REPORT_NOTIFICATION = 26;

	public static final int PRODUCT_NEWS = 27;

	public static final int BULK_UPDATE = 28;

	public static final int CMIS = 29;

	public static final int MOBILE_APPS = 30;

	public static final int EXTERNAL_CALL = 31;

	public static final int BULK_CHECKOUT = 32;

	public static final int WEBCONTENT = 33;

	public static final int CONTENT_DIFF = 34;

	public static final int UPDATES = 35;

	public static final int MOBILE_GUI = 36;

	public static final int BRANDING_FULL = 37;

	public static final int BRANDING_LOGO = 38;

	public static final int BRUTEFORCE_ATTACK_PREVENTION = 39;

	public static final int SHOW_LICENSEE = 40;

	public static final int FOLDER_TEMPLATE = 42;

	public static final int GDRIVE = 43;

	public static final int SMB_STORAGE = 44;

	public static final int FORMAT_CONVERSION = 45;
	
	public static final int CLOUD_STORAGE = 46;

	public static final int SHOW_DISABLED = 50;

	public static final int PREVIEW = 51;

	public static final int MULTI_STORAGE = 52;

	public static final int BOOKMARKS = 53;

	public static final int CUSTOMID = 54;

	public static final int LDAP = 55;

	public static final int TEMPLATE = 56;

	public static final int CLIENT_TOOLS = 57;

	public static final int WEBSERVICE = 58;

	public static final int WEBDAV = 59;

	public static final int EXPORT = 60;
	
	public static final int IMPORT_LOCAL_FOLDERS = 61;

	public static final int PARAMETRIC_SEARCHES = 62;

	public static final int DROP_SPOT = 63;

	public static final int BARCODES = 64;

	public static final int QUOTAS = 65;

	public static final int DUPLICATES_DISCOVERY = 66;

	public static final int COMPRESSED_REPO = 67;

	public static final int ADVANCED_OCR = 68;

	public static final int EXPORT_CSV = 69;

	public static final int RETENTION_POLICIES = 70;

	public static final int MULTI_WORKSPACE = 71;

	public static final int CLUSTERING = 72;

	public static final int FIREWALL = 73;

	public static final int CALENDAR = 74;

	public static final int CONNECTOR = 75;

	public static final int BACKUP = 76;

	public static final int KOFAX = 77;

	public static final int SYNC = 78;

	public static final int MULTI_TENANT = 79;

	public static final int DROPBOX = 80;

	public static final int ANTIVIRUS = 81;

	public static final int ANNOTATIONS = 82;

	public static final int ARCHIVING = 83;

	public static final int SHAREFILE = 84;

	public static final int STAMP = 85;

	public static final int FORM = 86;

	public static final int CUSTOM_REPORTS = 87;

	public static final int AUTO_NAMING = 88;

	public static final int AUTO_FOLDING = 89;

	public static final int HOT_FOLDER = 91;

	public static final int ZOHO = 92;

	public static final int WEBDAV_BASIC = 93;
	
	public static final int TWO_FACTORS_AUTHENTICATION = 94;

	private static Set<String> features = new HashSet<String>();

	static public void init(GUIInfo info) {
		features.clear();
		for (String feature : info.getFeatures()) {
			features.add(feature);
		}
	}

	public static boolean enabled(int feature) {
		String key = "Feature_" + feature;
		return features.contains(key);
	}

	public static boolean visible(int feature) {
		String key = "Feature_" + feature;
		if (features.contains(key))
			return true;
		else
			return showDisabled();
	}

	/**
	 * Check if a disabled feature must be visible(ad disabled) or hidden
	 */
	public static boolean showDisabled() {
		return enabled(SHOW_DISABLED);
	}

	/**
	 * Check if the licensee must be shown
	 */
	public static boolean showLicensee() {
		return enabled(SHOW_DISABLED);
	}
}