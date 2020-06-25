package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel shows the Sinlge-signon settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class SingleSignonPanel extends VLayout {

	public SingleSignonPanel() {
		setWidth100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get().loadSettingsByNames(new String[] { "cas.enabled", "cas.id", "cas.url",
				"cas.appurl", "cas.loginurl", "cas.group", "cas.lang", "cas.userType" },
				new AsyncCallback<GUIParameter[]>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIParameter[] settings) {
						initGUI(settings);
					}
				});
	}

	private void initGUI(GUIParameter[] settings) {
		final DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setAlign(Alignment.LEFT);

		RadioGroupItem enabled = ItemFactory.newBooleanSelector("enabled", I18N.message("enablecas"));
		enabled.setWrapTitle(false);
		enabled.setRequired(true);

		TextItem id = ItemFactory.newTextItem("id", "applicationid", null);
		id.setWidth(200);
		id.setWrapTitle(false);
		id.setRequired(true);

		TextItem appUrl = ItemFactory.newTextItem("appurl", "applicationurl", null);
		appUrl.setWidth(400);
		appUrl.setWrapTitle(false);
		appUrl.setRequired(true);

		TextItem casUrl = ItemFactory.newTextItem("casurl", "casurl", null);
		casUrl.setWidth(400);
		casUrl.setWrapTitle(false);
		casUrl.setRequired(true);

		TextItem casLoginUrl = ItemFactory.newTextItem("loginurl", "casloginurl", null);
		casLoginUrl.setWidth(400);
		casLoginUrl.setWrapTitle(false);
		casLoginUrl.setRequired(true);

		SelectItem group = ItemFactory.newGroupSelector("group", "group");
		group.setHint(I18N.message("casgrouphint"));
		group.setWrapTitle(false);
		group.setRequired(true);

		SelectItem language = ItemFactory.newLanguageSelector("language", false, true);
		language.setHint(I18N.message("caslanghint"));
		language.setWrapTitle(false);
		language.setRequired(true);

		SelectItem userType = ItemFactory.newUserTypeSelector("usertype", GUIUser.TYPE_DEFAULT);
		userType.setHint(I18N.message("casusertypehint"));
		userType.setWrapTitle(false);
		userType.setRequired(true);

		for (GUIParameter setting : settings) {
			if ("cas.enabled".equals(setting.getName()))
				enabled.setValue("true".equals(setting.getValue()) ? "yes" : "no");
			else if ("cas.id".equals(setting.getName()))
				id.setValue(setting.getValue());
			else if ("cas.loginurl".equals(setting.getName()))
				casLoginUrl.setValue(setting.getValue());
			else if ("cas.appurl".equals(setting.getName()))
				appUrl.setValue(setting.getValue());
			else if ("cas.url".equals(setting.getName()))
				casUrl.setValue(setting.getValue());
			else if ("cas.group".equals(setting.getName()))
				group.setValue(setting.getValue());
			else if ("cas.lang".equals(setting.getName()))
				language.setValue(setting.getValue());
			else if ("cas.userType".equals(setting.getName()))
				userType.setValue(setting.getValue());
		}

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				if (form.validate()) {
					GUIParameter[] params = new GUIParameter[8];
					params[0] = new GUIParameter("cas.enabled", "" + ("yes".equals(form.getValueAsString("enabled"))));
					params[1] = new GUIParameter("cas.id", form.getValueAsString("id").trim());
					params[2] = new GUIParameter("cas.url", form.getValueAsString("casurl").trim());
					params[3] = new GUIParameter("cas.appurl", form.getValueAsString("appurl").trim());
					params[4] = new GUIParameter("cas.loginurl", form.getValueAsString("loginurl").trim());
					params[5] = new GUIParameter("cas.group", form.getValueAsString("group").trim());
					params[6] = new GUIParameter("cas.lang", form.getValueAsString("language").trim());
					params[7] = new GUIParameter("cas.userType", form.getValueAsString("usertype").trim());

					for (GUIParameter param : params)
						Session.get().setConfig(param.getName(), param.getValue());

					SettingService.Instance.get().saveSettings(params, new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(Void ret) {
							Log.info(I18N.message("settingssaved"), null);
							if ("yes".equals(form.getValueAsString("enabled")))
								SC.say(I18N.message("settingssaved") + "\n" + I18N.message("suggestedtorestart"));
						}
					});
				}
			}
		});

		form.setFields(enabled, id, appUrl, casUrl, casLoginUrl, group, language, userType);

		Tab tab = new Tab();
		tab.setTitle(I18N.message("singlesignon"));
		tab.setPane(form);

		TabSet tabs = new TabSet();
		tabs.setWidth100();
		tabs.setHeight100();
		tabs.setTabs(tab);

		setMembers(tabs, save);
	}
}