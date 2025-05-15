package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Proxy settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ProxyPanel extends AdminPanel {

	private static final String PORT = "port";

	private static final String PASSWORD = "password";

	public ProxyPanel() {
		super("proxy");
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get().loadSettingsByNames(
				Arrays.asList("proxy.enabled", "proxy.host", "proxy.port", "proxy.password", "proxy.username"),
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(List<GUIParameter> parameters) {
						init(parameters);
					}
				});
	}

	private void init(List<GUIParameter> parameters) {
		DynamicForm form = new DynamicForm();
		form.setWidth(300);
		form.setColWidths(1, "*");
		form.setTitleOrientation(TitleOrientation.LEFT);

		ToggleItem enabled = ItemFactory.newToggleItem("eenabled", "enabled",
				Util.getParameterValueAsBoolean(parameters, "proxy.enabled"));

		TextItem host = ItemFactory.newTextItem("host", Util.getParameterValue(parameters, "proxy.host", "localhost"));
		host.setRequired(true);

		IntegerItem port = ItemFactory.newIntegerItem(PORT, I18N.message(PORT),
				Util.getParameterValueAsInt(parameters, "proxy.port", 0));
		port.setRequired(true);

		TextItem username = ItemFactory.newTextItem("username", Util.getParameterValue(parameters, "proxy.username"));

		PasswordItem password = ItemFactory.newPasswordItemPreventAutocomplete(PASSWORD, PASSWORD,
				Util.getParameterValue(parameters, "proxy.password"));

		form.setItems(enabled, host, port, username, password);

		IButton save = new IButton(I18N.message("save"));
		save.addClickHandler(click -> {
			if (Boolean.TRUE.equals(form.validate())) {
				List<GUIParameter> settings = new ArrayList<>();
				settings.add(new GUIParameter("proxy.enabled", form.getValueAsString("eenabled")));
				settings.add(new GUIParameter("proxy.host", form.getValueAsString("host")));
				settings.add(new GUIParameter("proxy.port", form.getValueAsString(PORT)));
				settings.add(new GUIParameter("proxy.username", form.getValueAsString("username")));
				settings.add(new GUIParameter("proxy.password", form.getValueAsString(PASSWORD)));

				SettingService.Instance.get().saveSettings(settings, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						GuiLog.info(I18N.message("settingssaved"), null);
					}
				});
			}
		});

		IButton test = new IButton(I18N.message("testconnection"));
		test.addClickHandler(click -> {
			LD.contactingServer();
			SettingService.Instance.get().testProxy(form.getValueAsString("host"),
					Integer.parseInt(form.getValueAsString(PORT)), form.getValueAsString("username"),
					form.getValueAsString(PASSWORD), new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Boolean connected) {
							LD.clearPrompt();
							if (connected.booleanValue())
								SC.say(I18N.message("connectionestablished"));
							else
								SC.warn(I18N.message("connectionfailed"));
						}
					});
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(2);
		buttons.setMembers(save, test);

		body.setMembers(form, buttons);
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}