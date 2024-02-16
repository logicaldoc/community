package com.logicaldoc.gui.setup.client;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.RootPanel;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.InfoService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.setup.client.services.SetupService;
import com.logicaldoc.gui.setup.client.services.SetupServiceAsync;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.RequiredIfValidator;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * The Setup entry point used for initial installation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Setup implements EntryPoint {

	private static final String SELECT_1 = "SELECT 1";

	private static final String EMBEDDED = "embedded";

	private static final String REG_EMAIL = "regEmail";

	private static final String REG_WEBSITE = "regWebsite";

	private static final String REG_ORGANIZATION = "regOrganization";

	private static final String REG_NAME = "regName";

	private static final String ORACLE = "Oracle";

	private static final String MYSQL = "MySQL";

	private static final String MARIADB = "MariaDB";

	private static final String SQLSERVER = "MSSQL";

	private static final String POSTGRESQL = "PostgreSQL";

	private static final String REPOSITORY_FOLDER = "repositoryFolder";

	private static final String LANGUAGE = "language";

	private static final String DB_TYPE = "dbType";

	private static final String DB_ENGINE = "dbEngine";

	private static final String DB_PASSWORD = "dbPassword";

	private static final String DB_USERNAME = "dbUsername";

	private static final String DB_URL = "dbUrl";

	private static final String DB_DRIVER = "dbDriver";

	int step = 0;

	private IButton submit;

	private ValuesManager vm = new ValuesManager();

	private TabSet tabs;

	private Map<String, String[]> engines = new HashMap<>();

	protected SetupServiceAsync setupService = (SetupServiceAsync) GWT.create(SetupService.class);

	@Override
	public void onModuleLoad() {
		String lang = Util.detectLocale();
		I18N.setLocale(lang);

		// Get grid of scrollbars, and clear out the window's built-in margin,
		// because we want to take advantage of the entire client area.
		Window.enableScrolling(false);
		Window.setMargin("0px");

		InfoService.Instance.get().getInfo(I18N.getLocale(), Constants.TENANT_DEFAULTNAME, true,
				new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable error) {
						SC.warn(error.getMessage());
					}

					@Override
					public void onSuccess(final GUIInfo info) {
						setupService.securityCheck(new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable error) {
								SC.warn(error.getMessage());
							}

							@Override
							public void onSuccess(Void arg0) {
								I18N.init(info);
								initGUI(info);
							}
						});

					}
				});

	}

	private void initGUI(final GUIInfo info) {
		WindowUtils.setTitle(info.getBranding().getProductName() + " " + info.getRelease()
				+ (info.getLicensee() != null ? " - " + I18N.message("licensedto") + ": " + info.getLicensee() : ""));
		WindowUtils.setFavicon(info);

		// Prepare a value manager that will include all forms spanned in each
		// tab
		vm = new ValuesManager();

		// Create all the tabs each one for a specific setup step
		tabs = new TabSet();
		tabs.setWidth(600);
		tabs.setHeight(250);

		Tab registrationTab = setupRegistration(vm);
		Tab repositoryTab = setupRepository(vm, info);
		Tab databaseTab = setupDatabase(vm, info);
		tabs.setTabs(registrationTab, repositoryTab, databaseTab);

		// This is the button used to confirm each step
		submit = new IButton();
		submit.setTitle(I18N.message("next"));
		submit.addClickHandler(event -> onSubmit(info));

		// Prepare the heading panel with Logo and Title
		// Prepare the logo image to be shown inside the login form
		Label header = new Label(I18N.message("setup"));
		header.setStyleName("setupHeader");
		header.setIcon(info.getBranding().getLogoSrc());
		header.setIconWidth(205);
		header.setIconHeight(40);
		header.setHeight(45);

		// Prepare a panel to layout setup components
		VLayout layout = new VLayout();
		layout.setHeight(500);
		layout.setWidth(400);
		layout.setMembersMargin(5);
		layout.addMember(header);
		layout.addMember(tabs);
		layout.addMember(submit);

		// Panel for horizontal centering
		VLayout vPanel = new VLayout();
		vPanel.setDefaultLayoutAlign(Alignment.CENTER);
		vPanel.setWidth100();
		vPanel.setHeight(300);
		vPanel.addMember(layout);

		RootPanel.get().add(vPanel);

		// Remove the loading frame
		RootPanel.getBodyElement().removeChild(RootPanel.get("loadingwrapper-setup").getElement());
	}

	/**
	 * Prepares the repository form
	 */
	private Tab setupRepository(final ValuesManager vm, GUIInfo info) {
		// Prepare the tab used to configure the repository where documents and
		// other data will be stored
		Tab repositoryTab = new Tab();
		repositoryTab.setTitle(I18N.message("repository"));

		final DynamicForm repositoryForm = new DynamicForm();
		repositoryForm.setID("repositoryForm");
		repositoryForm.setValuesManager(vm);
		String dir = info.getConfig("store.1.dir");
		if (dir != null && dir.endsWith("/"))
			dir = dir.substring(0, dir.length() - 1);
		while (dir != null && dir.endsWith("/docs"))
			dir = dir.substring(0, dir.length() - 5);
		TextItem repositoryItem = ItemFactory.newTextItem(REPOSITORY_FOLDER, "repofolder", dir);
		repositoryItem.setWidth(400);
		repositoryItem.setRequired(true);
		repositoryItem.setWrapTitle(false);
		repositoryForm.setFields(repositoryItem);
		repositoryForm.setDisabled(true);

		repositoryTab.setPane(repositoryForm);
		return repositoryTab;
	}

	/**
	 * Prepares the database tab
	 */
	private Tab setupDatabase(final ValuesManager vm, GUIInfo info) {
		// Prepare the fieldsMap with all database engines
		engines.put(MYSQL, new String[] { MYSQL, "com.mysql.cj.jdbc.Driver",
				"jdbc:mysql://<server>[,<failoverhost>][<:3306>]/<database>?useSSL=false&allowPublicKeyRetrieval=true&serverTimezone=UTC",
				"org.hibernate.dialect.MySQLDialect", SELECT_1 });
		engines.put(MARIADB, new String[] { MARIADB, "org.mariadb.jdbc.Driver",
				"jdbc:mariadb://<server>[<:3306>]/<database>", "org.hibernate.dialect.MySQLDialect", SELECT_1 });
		engines.put(POSTGRESQL,
				new String[] { POSTGRESQL, "org.postgresql.Driver", "jdbc:postgresql:[<//server>[<:5432>/]]<database>",
						"org.hibernate.dialect.PostgreSQLDialect", SELECT_1 });
		engines.put(ORACLE,
				new String[] { ORACLE, "oracle.jdbc.driver.OracleDriver", "jdbc:oracle:thin:@<server>[<:1521>]:<sid>",
						"org.hibernate.dialect.Oracle10gDialect", "SELECT 1 FROM DUAL" });
		engines.put(SQLSERVER,
				new String[] { "SQL Server", "com.microsoft.sqlserver.jdbc.SQLServerDriver",
						"jdbc:sqlserver://<server>[:<1433>];databaseName=<database>;instanceName=<instance>",
						"org.hibernate.dialect.SQLServer2008Dialect", SELECT_1 });

		Tab databaseTab = new Tab();
		databaseTab.setTitle(I18N.message("database"));

		final DynamicForm databaseForm = new DynamicForm();
		databaseForm.setWidth(450);
		databaseForm.setID("database");
		databaseForm.setValuesManager(vm);
		databaseForm.setDisabled(true);

		RadioGroupItem dbType = new RadioGroupItem();
		dbType.setName(DB_TYPE);
		dbType.setWrapTitle(false);
		dbType.setRequired(true);
		dbType.setVertical(false);
		dbType.setValueMap(I18N.message(EMBEDDED), I18N.message("external"));
		dbType.setRedrawOnChange(true);
		dbType.setTitle(I18N.message("dbtype"));
		dbType.setValue(
				"hsqldb".equals(info.getConfig("jdbc.dbms")) ? I18N.message(EMBEDDED) : I18N.message("external"));

		// The database engine, if the External db was chosen
		SelectItem dbEngine = new SelectItem();
		dbEngine.setTitle(I18N.message("dbengine"));
		dbEngine.setWrapTitle(false);
		dbEngine.setVisible(false);
		dbEngine.setName(DB_ENGINE);
		LinkedHashMap<String, String> valueMap = new LinkedHashMap<>();
		for (Map.Entry<String, String[]> entry : engines.entrySet())
			valueMap.put(entry.getKey(), entry.getValue()[0]);

		dbEngine.setValueMap(valueMap);
		dbEngine.setValue(MYSQL);
		dbEngine.setShowIfCondition(
				(item, value, form) -> !I18N.message(EMBEDDED).equals(databaseForm.getValue(DB_TYPE)));
		RequiredIfValidator ifValidator = new RequiredIfValidator();
		ifValidator.setExpression((formItem, value) -> !I18N.message(EMBEDDED).equals(databaseForm.getValue(DB_TYPE)));
		dbEngine.setValidators(ifValidator);
		dbEngine.addChangeHandler(event -> {
			String selectedItem = (String) event.getValue();
			databaseForm.getField(DB_DRIVER).setValue(engines.get(selectedItem)[1]);
			databaseForm.getField(DB_URL).setValue(engines.get(selectedItem)[2]);
		});

		// The driver for the external DB
		TextItem dbDriver = ItemFactory.newTextItem(DB_DRIVER, "driverclass", info.getConfig("jdbc.driver"));
		dbDriver.setVisible(false);
		dbDriver.setWrapTitle(false);
		dbDriver.setWidth(300);
		dbDriver.setShowIfCondition(
				(item, value, form) -> !I18N.message(EMBEDDED).equals(databaseForm.getValue(DB_TYPE)));
		dbDriver.setValidators(ifValidator);

		// The connection URL to external DB
		TextItem dbUrl = ItemFactory.newTextItem(DB_URL, "connectionurl", info.getConfig("jdbc.url"));
		dbUrl.setWidth(400);
		dbUrl.setVisible(false);
		dbUrl.setWrapTitle(false);
		dbUrl.setShowIfCondition((item, value, form) -> !I18N.message(EMBEDDED).equals(databaseForm.getValue(DB_TYPE)));
		dbUrl.setValidators(ifValidator);

		// The username to access the external DB
		TextItem dbUsername = ItemFactory.newTextItem(DB_USERNAME, "username", info.getConfig("jdbc.username"));
		dbUsername.setVisible(false);
		dbUsername.setWrapTitle(false);
		dbUsername.setShowIfCondition(
				(item, value, form) -> !I18N.message(EMBEDDED).equals(databaseForm.getValue(DB_TYPE)));

		// The password to access the external DB
		PasswordItem dbPassword = new PasswordItem();
		dbPassword.setVisible(false);
		dbPassword.setTitle(I18N.message("password"));
		dbPassword.setName(DB_PASSWORD);
		dbPassword.setWrapTitle(false);
		dbPassword.setValue(info.getConfig("jdbc.password"));
		dbPassword.setShowIfCondition(
				(item, value, form) -> !I18N.message(EMBEDDED).equals(databaseForm.getValue(DB_TYPE)));

		databaseForm.setFields(dbType, dbEngine, dbDriver, dbUrl, dbUsername, dbPassword);
		databaseTab.setPane(databaseForm);
		return databaseTab;
	}

	/**
	 * Prepares the registration form
	 */
	private Tab setupRegistration(final ValuesManager vm) {
		Tab registrationTab = new Tab();
		registrationTab.setTitle(I18N.message("registration"));

		TextItem regName = ItemFactory.newTextItem(REG_NAME, "name", null);
		regName.setWrapTitle(false);
		regName.setWidth(300);

		TextItem regOrganization = ItemFactory.newTextItem(REG_ORGANIZATION, "organization", null);
		regOrganization.setWrapTitle(false);
		regOrganization.setWidth(300);

		TextItem regWebsite = ItemFactory.newTextItem(REG_WEBSITE, "website", null);
		regWebsite.setWrapTitle(false);
		regWebsite.setWidth(300);

		TextItem regEmail = ItemFactory.newEmailItem(REG_EMAIL, "email", false);
		regEmail.setWrapTitle(false);
		regEmail.setWidth(300);

		SelectItem languageItem = ItemFactory.newLanguageSelector(LANGUAGE, false, true);
		languageItem.setTitle(I18N.message(LANGUAGE));
		languageItem.setRequired(true);

		final DynamicForm regForm = new DynamicForm();
		regForm.setID("regForm");
		regForm.setValuesManager(vm);
		regForm.setFields(languageItem, regName, regEmail, regOrganization, regWebsite);

		registrationTab.setPane(regForm);
		return registrationTab;
	}

	private void onSubmit(final GUIInfo info) {
		try {
			int tabIndex = tabs.getSelectedTabNumber();

			if (Boolean.FALSE.equals(vm.validate()))
				return;

			if (step == tabs.getTabs().length - 1) {
				if (Boolean.FALSE.equals(vm.validate()))
					SC.warn("invalidfields");

				SetupInfo data = new SetupInfo();
				data.setDbDriver(vm.getValueAsString(DB_DRIVER));
				data.setDbUrl(vm.getValueAsString(DB_URL));
				data.setDbUsername(vm.getValueAsString(DB_USERNAME));
				data.setDbPassword(vm.getValueAsString(DB_PASSWORD));
				data.setDbEngine(vm.getValueAsString(DB_ENGINE));
				data.setDbType(vm.getValueAsString(DB_TYPE));
				data.setLanguage(vm.getValueAsString(LANGUAGE));
				data.setRepositoryFolder(vm.getValueAsString(REPOSITORY_FOLDER));
				data.setRegEmail(vm.getValueAsString(REG_EMAIL));
				data.setRegName(vm.getValueAsString(REG_NAME));
				data.setRegOrganization(vm.getValueAsString(REG_ORGANIZATION));
				data.setRegWebsite(vm.getValueAsString(REG_WEBSITE));

				if (I18N.message(EMBEDDED).equals(data.getDbType())) {
					data.setDbEngine("hsqldb");
					data.setDbDriver("org.hsqldb.jdbcDriver");
					data.setDbUrl(("jdbc:hsqldb:" + data.getRepositoryFolder() + "/db/db").replace("//", "/"));
					data.setDbUsername("sa");
					data.setDbPassword("");
					data.setDbDialect("org.hibernate.dialect.HSQLDialect");
					data.setDbValidationQuery("SELECT 1 FROM INFORMATION_SCHEMA.SYSTEM_USERS");
				} else {
					data.setDbDialect(engines.get(data.getDbEngine())[3]);
					data.setDbValidationQuery(engines.get(data.getDbEngine())[4]);
				}

				LD.contactingServer();
				setupService.setup(data, new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						SC.warn(caught.getMessage());
						submit.setDisabled(false);
					}

					@Override
					public void onSuccess(Void arg) {
						LD.clearPrompt();
						SC.say(I18N.message("installationperformed"),
								I18N.message("installationend", info.getBranding().getProduct()),
								value -> Util.redirect(Util.contextPath()));
					}
				});
				submit.setDisabled(true);
			} else {
				// Go to the next tab and enable the contained panel
				tabs.selectTab(tabIndex + 1);
				tabs.getSelectedTab().getPane().setDisabled(false);
				if (step < tabs.getSelectedTabNumber())
					step++;
				if (step == tabs.getTabs().length - 1)
					submit.setTitle(I18N.message("setup"));
			}
		} catch (Exception e) {
			SC.warn("Error: " + e.getMessage());
		}
	}
}