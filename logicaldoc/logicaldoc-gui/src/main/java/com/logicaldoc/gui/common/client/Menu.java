package com.logicaldoc.gui.common.client;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.gui.common.client.beans.GUIUser;

/**
 * Stores the accessible menus
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Menu {

	public static final long DOWNLOAD_TICKETS = -7;
	
	public static final long DELETED_FOLDERS = -6;

	public static final long DELETED_DOCS = -5;

	public static final long ARCHIVED_DOCS = -4;

	public static final long LOCKED_DOCS = -3;

	public static final long LAST_CHANGES = -2;

	public static final long ADMINISTRATION = 2;

	public static final long ACCOUNT = 40;
	
	public static final long DOCUMENTS = 1500;
	
	public static final long HISTORY = 1600;

	public static final long TRASH = 1602;
	
	public static final long VERSIONS = 1603;

	public static final long ALIASES = 1605;
	
	public static final long DOCUMENT_CALENDAR = 1606;
	
	public static final long SIGNATURE = 1607;
	
	public static final long CAPTURE = 1608;
	
	public static final long PREVIEW = 1609;
	
	public static final long RATING = 1610;
	
	public static final long DASHBOARD_CALENDAR = 1526;

	public static final long SEARCH = 1510;
	
	public static final long SHARE_SEARCH = 1511;

	public static final long DASHBOARD = 1520;
	
	public static final long MESSAGES = 1525;
	
	public static final long CHAT = 1527;

	public static final long CONTACTS = 1530;

	public static final long INTERFACE_DENSITY = 1535;

	public static final long STAMPS = 1540;

	public static final long FORMS = 1550;

	public static final long SECURITY = 9;

	public static final long CLIENTS = 3;

	public static final long SETTINGS = 7;

	public static final long IMPEX = 8;
	
	public static final long INDEX = 10;

	public static final long FOLDER_INTERFACE = 11;
	
	public static final long DROP_SPOT = 12;
	
	public static final long CUSTOM_ID = 17;

	public static final long METADATA = 25;

	public static final long WORKFLOW = 23;
	
	public static final long DASHBOARD_WORKFLOW = 1580;
	
	public static final long DASHBOARD_WORKFLOW_ALLWORKFLOWS = 1581;
	
	public static final long DASHBOARD_WORKFLOW_WORKFLOWSINVOLVEDIN = 1582;
	
	public static final long DASHBOARD_WORKFLOW_WORKFLOWSYOUSUPERVISE = 1583;

	public static final long BARCODES = 30;

	public static final long REPORTS = 90;

	public static final long CUSTOMREPORTS = 1560;

	public static final long PARAMETERS = 100;

	public static final long REPOSITORIES = 105;

	public static final long OFFICE = -1090;

	public static final long CLUSTERING = -1110;

	public static final long SUBSCRIPTIONS = -1120;

	public static final long UPDATES_AND_PATCHES = -1130;

	public static final long TENANTS = -1140;

	public static final long BRANDING = -1150;

	public static final long KEYSTORE = -1160;
	
	public static final long VIA = -1170;
	
	public static final long COMPARATORS = 1660;
	
	public static final long AUTOMATION = 1570;

	public static final long CALENDAR_REPORT = -2060;

	public static final long DROPBOX = -2070;

	public static final long SHAREFILE = -2075;

	public static final long GDOCS = -2080;

	public static final long WEBCONTENT = -2100;

	public static final long TEXTCONTENT = 200;

	public static final long RETENTION_POLICIES = -2110;

	public static final long ZOHO = -2120;

	public static final long DOCUSIGN = -2125;
	
	public static final long SYSTEM = 80;

	public static final long GENERAL = 70;

	public static final long ADMIN_SESSIONS = 71;

	public static final long LOGS = 72;

	public static final long RUNLEVEL = 73;

	public static final long SESSIONS = 1601;

	public static final long QUOTA = 1700;

	public static final long FORMAT_CONVERTERS = 1750;

	public static final long FIREWALL = 1805;

	public static final long BRUTEFORCE_ATTACK_PREVENTION = 1820;

	public static final long TWO_FACTORS_AUTHENTICATION = 1870;
	
	public static final long EXTERNAL_AUTHENTICATION = 1880;

	public static final long SINGLE_SIGNON = 1875;
	
	public static final long RESTART = -1190;
	
	public static final long SYNDICATION = 2000;
	
	public static final long ZONAL_OCR = 2100;
	
	public static final long CUSTOM_ACTIONS = 1300;
	
	public static final long SUBSCRIPTIONS_REPORT = -1125;
	
	public static final long SCAN = 550;
	
	private static Set<Long> menus = new HashSet<Long>();

	static public void init(GUIUser user) {
		menus.clear();
		for (long menu : user.getMenus()) {
			menus.add(menu);
		}
	}

	public static boolean enabled(long menu) {
		return menus.contains(menu);
	}
}