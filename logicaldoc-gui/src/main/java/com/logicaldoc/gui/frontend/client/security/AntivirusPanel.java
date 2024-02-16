package com.logicaldoc.gui.frontend.client.security;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel shows the Antivirus settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class AntivirusPanel extends VLayout {

	private static final String ENABLED = "enabled";

	private static final String ANTIVIRUS_EXCLUDES = ".antivirus.excludes";

	private static final String ANTIVIRUS_INCLUDES = ".antivirus.includes";

	private static final String ANTIVIRUS_TIMEOUT = ".antivirus.timeout";

	private static final String ANTIVIRUS_ENABLED = ".antivirus.enabled";

	private static final String ANTIVIRUS_COMMAND = "antivirus.command";

	private DynamicForm form = new DynamicForm();

	public AntivirusPanel() {
		setWidth100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	protected void onDraw() {
		String tenant = Session.get().getTenantName();
		SettingService.Instance.get().loadSettingsByNames(
				Arrays.asList(ANTIVIRUS_COMMAND, tenant + ANTIVIRUS_ENABLED, tenant + ANTIVIRUS_INCLUDES,
						tenant + ANTIVIRUS_EXCLUDES, tenant + ANTIVIRUS_TIMEOUT),
				new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(List<GUIParameter> parameters) {
						initGUI(parameters);
					}
				});

	}

	private void initGUI(List<GUIParameter> settings) {
		prepareForm(settings);

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(click -> {
			if (form.validate()) {
				List<GUIParameter> params = new ArrayList<>();
				params.add(new GUIParameter(Session.get().getTenantName() + ANTIVIRUS_ENABLED,
						"" + ("yes".equals(form.getValueAsString(ENABLED)))));
				params.add(new GUIParameter(Session.get().getTenantName() + ANTIVIRUS_EXCLUDES,
						form.getValueAsString("excludes").trim()));
				params.add(new GUIParameter(Session.get().getTenantName() + ANTIVIRUS_INCLUDES,
						form.getValueAsString("includes").trim()));
				params.add(new GUIParameter(Session.get().getTenantName() + ANTIVIRUS_TIMEOUT,
						form.getValueAsString("timeout").trim()));

				if (Session.get().isDefaultTenant())
					params.add(new GUIParameter(ANTIVIRUS_COMMAND, form.getValueAsString("command").trim()));

				SettingService.Instance.get().saveSettings(params, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void ret) {
						GuiLog.info(I18N.message("settingssaved"), null);
					}
				});
			}
		});

		Tab tab = new Tab();
		tab.setTitle(I18N.message("antivirus"));
		tab.setPane(form);

		TabSet tabs = new TabSet();
		tabs.setWidth100();
		tabs.setHeight100();
		tabs.setTabs(tab);

		setMembers(tabs, save);
	}

	private void prepareForm(List<GUIParameter> settings) {
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setAlign(Alignment.LEFT);

		RadioGroupItem enabled = ItemFactory.newBooleanSelector(ENABLED, I18N.message(ENABLED));
		enabled.setWrapTitle(false);
		enabled.setRequired(true);

		TextItem command = ItemFactory.newTextItem("command", "ClamAV", null);
		command.setWidth(400);

		TextItem includes = ItemFactory.newTextItem("includes", "include", null);
		includes.setWidth(400);

		TextItem excludes = ItemFactory.newTextItem("excludes", "exclude", null);
		excludes.setWidth(400);

		TextItem timeout = ItemFactory.newSpinnerItem("timeout", (Integer) null);
		timeout.setHint(I18N.message("seconds"));

		for (GUIParameter setting : settings) {
			if ((Session.get().getTenantName() + ANTIVIRUS_ENABLED).equals(setting.getName()))
				enabled.setValue("true".equals(setting.getValue()) ? "yes" : "no");
			else if (ANTIVIRUS_COMMAND.equals(setting.getName()))
				command.setValue(setting.getValue());
			else if ((Session.get().getTenantName() + ANTIVIRUS_EXCLUDES).equals(setting.getName()))
				excludes.setValue(setting.getValue());
			else if ((Session.get().getTenantName() + ANTIVIRUS_INCLUDES).equals(setting.getName()))
				includes.setValue(setting.getValue());
			else if ((Session.get().getTenantName() + ANTIVIRUS_TIMEOUT).equals(setting.getName()))
				timeout.setValue(Integer.parseInt(setting.getValue()));
		}

		if (Session.get().isDefaultTenant())
			form.setFields(enabled, command, includes, excludes, timeout);
		else
			form.setFields(enabled, includes, excludes, timeout);
	}
}