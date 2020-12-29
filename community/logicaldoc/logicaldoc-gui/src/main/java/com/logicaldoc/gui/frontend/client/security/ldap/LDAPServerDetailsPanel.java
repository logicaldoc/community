package com.logicaldoc.gui.frontend.client.security.ldap;

import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUILDAPServer;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.services.LDAPService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel shows the LDAP and Active Directory settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class LDAPServerDetailsPanel extends VLayout {

	private ValuesManager vm = new ValuesManager();

	private TabSet tabs = new TabSet();

	private GUILDAPServer server;

	private LDAPBrowser browser;

	public LDAPServerDetailsPanel(LDAPServersPanel listing, GUILDAPServer server) {
		this.server = server;

		setWidth100();
		setMembersMargin(5);
		setMargin(5);

		tabs.setWidth100();
		tabs.setHeight100();

		Tab ldapTab = new Tab();
		ldapTab.setTitle(I18N.message("server"));

		Tab browserTab = new Tab();
		browserTab.setTitle(I18N.message("browser"));

		DynamicForm ldapForm = new DynamicForm();
		ldapForm.setValuesManager(vm);
		ldapForm.setTitleOrientation(TitleOrientation.TOP);
		ldapForm.setColWidths(100, 100);

		// Implementation
		RadioGroupItem implementation = ItemFactory.newBooleanSelector("implementation", "implementation");
		implementation.setName("implementation");
		implementation.setValueMap("basic", "md5");
		implementation.setRequired(true);
		implementation.setValue(this.server.getImplementation());

		// Enabled
		RadioGroupItem enabled = ItemFactory.newBooleanSelector("eenabled", "enabled");
		enabled.setValue(this.server.isEnabled() ? "yes" : "no");
		enabled.setCellStyle("warn");
		enabled.setRequired(true);

		// Anonymous Login
		RadioGroupItem anon = ItemFactory.newBooleanSelector("anon", "anonymous");
		anon.setValue(this.server.isAnonymous() ? "yes" : "no");
		anon.setRequired(true);

		// Url
		TextItem url = ItemFactory.newTextItem("url", "ldaphurl", this.server.getUrl());
		url.setRequired(true);
		url.setCellStyle("warn");
		url.setWidth(300);

		// Username
		TextItem username = ItemFactory.newTextItem("username", "user", this.server.getUsername());
		username.setCellStyle("warn");
		username.setWidth(300);

		// Password
		PasswordItem password = new PasswordItem("password", I18N.message("password"));
		password.setName("password");
		password.setValue(this.server.getPassword());
		password.setCellStyle("warn");
		password.setWidth(300);

		// User type
		SelectItem userType = ItemFactory.newUserTypeSelector("usertype", this.server.getUserType());

		// Realm
		TextItem realm = ItemFactory.newTextItem("realm", "realm", this.server.getRealm());
		realm.setWidth(300);

		// User identifier attr.
		TextItem userIdentifierAttr = ItemFactory.newTextItem("useridentifierattr", "useridentifierattr",
				this.server.getUserIdentifierAttr());
		userIdentifierAttr.setWidth(300);

		// Group identifier attr.
		TextItem grpIdentifierAttr = ItemFactory.newTextItem("grpidentifierattr", "grpidentifierattr",
				this.server.getGroupIdentifierAttr());
		grpIdentifierAttr.setWidth(300);

		// Logon attr.
		TextItem logonAttr = ItemFactory.newTextItem("logonattr", "logonattr", this.server.getLogonAttr());
		logonAttr.setWidth(300);

		// User class
		TextItem userClass = ItemFactory.newTextItem("userclass", "userclass", this.server.getUserClass());
		userClass.setWidth(300);

		// Group class
		TextItem groupClass = ItemFactory.newTextItem("grpclass", "grpclass", this.server.getGroupClass());
		groupClass.setWidth(300);

		// Users base node
		TextItem usersBaseNode = ItemFactory.newTextItem("usersbasenode", "usersbasenode", this.server.getUserNodes());
		usersBaseNode.setWidth(300);

		// User filters
		TextItem userInclude = ItemFactory.newTextItem("userinclude", "userinclusionfilers",
				this.server.getUserIncludes());
		userInclude.setWidth(300);
		TextItem userExclude = ItemFactory.newTextItem("userexclude", "userexclusionfilers",
				this.server.getUserExcludes());
		userExclude.setWidth(300);

		// Groups base node
		TextItem groupsBaseNode = ItemFactory.newTextItem("grpsbasenode", "grpsbasenode", this.server.getGroupNodes());
		groupsBaseNode.setWidth(300);

		// Group filters
		TextItem groupInclude = ItemFactory.newTextItem("groupinclude", "groupinclusionfilers",
				this.server.getGroupIncludes());
		groupInclude.setWidth(300);
		TextItem groupExclude = ItemFactory.newTextItem("groupexclude", "groupexclusionfilers",
				this.server.getGroupExcludes());
		groupExclude.setWidth(300);

		// Page size
		SpinnerItem pageSize = ItemFactory.newSpinnerItem("pagesize", "pagesize", this.server.getPageSize());
		pageSize.setRequired(true);
		pageSize.setMin(0);
		pageSize.setStep(50);
		
		// Timepout
		SpinnerItem timeout = ItemFactory.newSpinnerItem("timeout", "timeout", this.server.getTimeout());
		timeout.setRequired(true);
		timeout.setMin(1);
		timeout.setStep(5);
		timeout.setHint(I18N.message("seconds").toLowerCase());

		// Synch TTL
		SpinnerItem syncTtl = ItemFactory.newSpinnerItem("syncttl", "synchronizeifolderthan", this.server.getSyncTtl());
		syncTtl.setRequired(true);
		syncTtl.setHint(I18N.message("hours").toLowerCase());
		syncTtl.setMin(0);
		syncTtl.setStep(1);

		// Language
		SelectItem language = ItemFactory.newLanguageSelector("language", false, true);
		language.setName("language");
		language.setRequired(true);
		language.setValue(this.server.getLanguage());

		// Keep membership in local groups
		RadioGroupItem keepMembership = ItemFactory.newBooleanSelector("keepmembership", "keepmembershiplocalgroups");
		keepMembership.setValue(this.server.isKeepLocalMemberships() ? "yes" : "no");
		keepMembership.setRequired(true);
		keepMembership.setEndRow(true);

		
		TextAreaItem validation = ItemFactory.newTextAreaItemForAutomation("validation",
				"validation", this.server.getValidation(), null, false);
		validation.setHeight(150);
		validation.setWidth(400);
		validation.setWrapTitle(false);
		validation.setColSpan(2);
		
		ldapForm.setItems(enabled, url, username, password, implementation, anon, syncTtl, pageSize, timeout, language, userType,
				keepMembership, userIdentifierAttr, grpIdentifierAttr, userClass, groupClass, usersBaseNode,
				groupsBaseNode, userInclude, groupInclude, userExclude, groupExclude, logonAttr, realm, validation);

		ldapTab.setPane(ldapForm);

		if (Feature.visible(Feature.LDAP)) {
			tabs.addTab(ldapTab);
			tabs.addTab(browserTab);
			if (!Feature.enabled(Feature.LDAP)) {
				ldapTab.setPane(new FeatureDisabled());
				browserTab.setPane(new FeatureDisabled());
			} else {
				ldapTab.setPane(ldapForm);
				browserTab.setPane(new LDAPBrowser(server));
			}
		}

		IButton test = new IButton();
		test.setAutoFit(true);
		test.setTitle(I18N.message("testconnection"));
		test.setDisabled(server.getId() == 0L);
		test.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					LDAPServerDetailsPanel.this.server.setImplementation((String) values.get("implementation"));
					LDAPServerDetailsPanel.this.server.setEnabled(values.get("eenabled").equals("yes") ? true : false);
					LDAPServerDetailsPanel.this.server.setAnonymous(values.get("anon").equals("yes") ? true : false);
					LDAPServerDetailsPanel.this.server
							.setKeepLocalMemberships(values.get("keepmembership").equals("yes") ? true : false);
					LDAPServerDetailsPanel.this.server.setUrl((String) values.get("url"));
					LDAPServerDetailsPanel.this.server.setUsername((String) values.get("username"));
					LDAPServerDetailsPanel.this.server.setPassword((String) values.get("password"));
					LDAPServerDetailsPanel.this.server.setRealm((String) values.get("realm"));
					LDAPServerDetailsPanel.this.server.setUserIdentifierAttr((String) values.get("useridentifierattr"));
					LDAPServerDetailsPanel.this.server.setGroupIdentifierAttr((String) values.get("grpidentifierattr"));
					LDAPServerDetailsPanel.this.server.setLogonAttr((String) values.get("logonattr"));
					LDAPServerDetailsPanel.this.server.setUserClass((String) values.get("userclass"));
					LDAPServerDetailsPanel.this.server.setGroupClass((String) values.get("grpclass"));
					LDAPServerDetailsPanel.this.server.setUserNodes((String) values.get("usersbasenode"));
					LDAPServerDetailsPanel.this.server.setUserIncludes((String) values.get("userinclude"));
					LDAPServerDetailsPanel.this.server.setUserExcludes((String) values.get("userexclude"));
					LDAPServerDetailsPanel.this.server.setGroupNodes((String) values.get("grpsbasenode"));
					LDAPServerDetailsPanel.this.server.setGroupIncludes((String) values.get("groupinclude"));
					LDAPServerDetailsPanel.this.server.setGroupExcludes((String) values.get("groupexclude"));
					LDAPServerDetailsPanel.this.server.setLanguage((String) values.get("language"));
					LDAPServerDetailsPanel.this.server.setValidation((String) values.get("validation"));

					if (browser != null && browser instanceof LDAPBrowser)
						browser.setServer(LDAPServerDetailsPanel.this.server);

					listing.updateRecord(LDAPServerDetailsPanel.this.server);

					LDAPService.Instance.get().testConnection(LDAPServerDetailsPanel.this.server,
							new AsyncCallback<Boolean>() {

								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Boolean ret) {
									if (ret)
										SC.say(I18N.message("connectionestablished"));
									else
										SC.warn(I18N.message("connectionfailed"));
								}
							});
				}
			}
		});

		IButton activedir = new IButton();
		activedir.setAutoFit(true);
		activedir.setTitle(I18N.message("activedirectory"));
		activedir.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				LD.askForValue(I18N.message("activedirectory"), I18N.message("addomain"), "", new ValueCallback() {
					@Override
					public void execute(String value) {
						if (value == null)
							return;
						String node = value.replaceAll("\\.", ",DC=");
						node = "DC=" + node;
						vm.setValue("url", "ldap://AD_SERVER:389");
						vm.setValue("username", "CN=Administrator,CN=Users," + node);
						vm.setValue("useridentifierattr", "CN");
						vm.setValue("grpidentifierattr", "CN");
						vm.setValue("logonattr", "sAMAccountName");
						vm.setValue("userclass", "person");
						vm.setValue("grpclass", "group");
						vm.setValue("userclass", "person");
						vm.setValue("usersbasenode", "CN=Users," + node);
						vm.setValue("grpsbasenode", "CN=Builtin," + node);
						vm.setValue("anon", "no");
					}
				});
			}
		});

		IButton save = new IButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					LDAPServerDetailsPanel.this.server.setImplementation((String) values.get("implementation"));
					LDAPServerDetailsPanel.this.server.setEnabled(values.get("eenabled").equals("yes") ? true : false);
					LDAPServerDetailsPanel.this.server.setAnonymous(values.get("anon").equals("yes") ? true : false);
					LDAPServerDetailsPanel.this.server
							.setKeepLocalMemberships(values.get("keepmembership").equals("yes") ? true : false);
					LDAPServerDetailsPanel.this.server.setUrl((String) values.get("url"));
					LDAPServerDetailsPanel.this.server.setUsername((String) values.get("username"));
					LDAPServerDetailsPanel.this.server.setPassword((String) values.get("password"));
					LDAPServerDetailsPanel.this.server.setRealm((String) values.get("realm"));
					LDAPServerDetailsPanel.this.server.setUserIdentifierAttr((String) values.get("useridentifierattr"));
					LDAPServerDetailsPanel.this.server.setGroupIdentifierAttr((String) values.get("grpidentifierattr"));
					LDAPServerDetailsPanel.this.server.setLogonAttr((String) values.get("logonattr"));
					LDAPServerDetailsPanel.this.server.setUserClass((String) values.get("userclass"));
					LDAPServerDetailsPanel.this.server.setGroupClass((String) values.get("grpclass"));
					LDAPServerDetailsPanel.this.server.setUserNodes((String) values.get("usersbasenode"));
					LDAPServerDetailsPanel.this.server.setUserIncludes((String) values.get("userinclude"));
					LDAPServerDetailsPanel.this.server.setUserExcludes((String) values.get("userexclude"));
					LDAPServerDetailsPanel.this.server.setGroupNodes((String) values.get("grpsbasenode"));
					LDAPServerDetailsPanel.this.server.setGroupIncludes((String) values.get("groupinclude"));
					LDAPServerDetailsPanel.this.server.setGroupExcludes((String) values.get("groupexclude"));
					LDAPServerDetailsPanel.this.server.setPageSize(Integer.parseInt(values.get("pagesize").toString()));
					LDAPServerDetailsPanel.this.server.setSyncTtl(Integer.parseInt(values.get("syncttl").toString()));
					LDAPServerDetailsPanel.this.server.setLanguage((String) values.get("language"));
					LDAPServerDetailsPanel.this.server.setUserType(Integer.parseInt(values.get("usertype").toString()));
					LDAPServerDetailsPanel.this.server.setValidation((String) values.get("validation"));
					LDAPServerDetailsPanel.this.server.setTimeout(Integer.parseInt(values.get("timeout").toString()));
					
					LDAPService.Instance.get().save(LDAPServerDetailsPanel.this.server,
							new AsyncCallback<GUILDAPServer>() {

								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(GUILDAPServer server) {
									LDAPServerDetailsPanel.this.server = server;
									if (browser != null && browser instanceof LDAPBrowser)
										browser.setServer(LDAPServerDetailsPanel.this.server);
									listing.updateRecord(LDAPServerDetailsPanel.this.server);
									test.setDisabled(false);
									Log.info(I18N.message("settingssaved"), null);
								}
							});
				}
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(3);
		buttons.setMembers(save, activedir, test);
		setMembers(tabs, buttons);
	}
}