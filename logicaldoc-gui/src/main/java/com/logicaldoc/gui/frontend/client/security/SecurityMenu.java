package com.logicaldoc.gui.frontend.client.security;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.security.group.GroupsPanel;
import com.logicaldoc.gui.frontend.client.security.ldap.LDAPServersPanel;
import com.logicaldoc.gui.frontend.client.security.saml.SamlPanel;
import com.logicaldoc.gui.frontend.client.security.twofactorsauth.TwoFactorsAuthenticationSettings;
import com.logicaldoc.gui.frontend.client.security.user.UsersPanel;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the administration security menu
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SecurityMenu extends VLayout {

	public SecurityMenu() {
		setMargin(10);
		setMembersMargin(5);
		setOverflow(Overflow.AUTO);

		addUsersButton();

		addGroupsButton();

		addSecurityButton();

		addAntivirusButton();

		addFirewallButton();

		addBruteForceButton();

		addTwoFactorsAuthButton();

		addExtAuthButton();

		addSamlButton();
	}

	private void addSecurityButton() {
		Button security = new Button(I18N.message("security"));
		security.setWidth100();
		security.setHeight(25);
		security.addClickHandler(
				event -> SecurityService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUISecuritySettings settings) {
						AdminScreen.get().setContent(new SecuritySettingsPanel(settings));
					}

				}));
		addMember(security);
	}

	private void addGroupsButton() {
		Button groups = new Button(I18N.message("groups"));
		groups.setWidth100();
		groups.setHeight(25);
		groups.addClickHandler(event -> AdminScreen.get().setContent(new GroupsPanel()));
		addMember(groups);
	}

	private void addUsersButton() {
		Button users = new Button(I18N.message("users"));
		users.setWidth100();
		users.setHeight(25);
		users.addClickHandler(event -> AdminScreen.get().setContent(new UsersPanel()));
		addMember(users);
	}

	private void addSamlButton() {
		Button saml = new Button(I18N.message("singlesignonsaml"));
		saml.setWidth100();
		saml.setHeight(25);
		saml.addClickHandler(event -> AdminScreen.get().setContent(new SamlPanel()));
		if (Feature.visible(Feature.SINGLE_SIGNON) && Session.get().isDefaultTenant() && Menu.enabled(Menu.SAML)) {
			addMember(saml);
			if (!Feature.enabled(Feature.SINGLE_SIGNON) || Session.get().isDemo())
				setFeatureDisabled(saml);
		}
	}

	private void setFeatureDisabled(Button button) {
		button.setDisabled(true);
		button.setTooltip(I18N.message("featuredisabled"));
	}

	private void addExtAuthButton() {
		Button extAuth = new Button(I18N.message("extauth"));
		extAuth.setWidth100();
		extAuth.setHeight(25);
		extAuth.addClickHandler(event -> AdminScreen.get().setContent(new LDAPServersPanel()));
		if (Feature.visible(Feature.LDAP) && Menu.enabled(Menu.EXTERNAL_AUTHENTICATION)) {
			addMember(extAuth);
			if (!Feature.enabled(Feature.LDAP) || Session.get().isDemo())
				setFeatureDisabled(extAuth);
		}
	}

	private void addTwoFactorsAuthButton() {
		Button twoFactorsAuthentication = new Button(I18N.message("twofactorsauth"));
		twoFactorsAuthentication.setWidth100();
		twoFactorsAuthentication.setHeight(25);
		twoFactorsAuthentication
				.addClickHandler(event -> AdminScreen.get().setContent(new TwoFactorsAuthenticationSettings()));
		if (Feature.visible(Feature.TWO_FACTORS_AUTHENTICATION) && Menu.enabled(Menu.TWO_FACTORS_AUTHENTICATION)) {
			addMember(twoFactorsAuthentication);
			if (!Feature.enabled(Feature.TWO_FACTORS_AUTHENTICATION) || Session.get().isDemo())
				setFeatureDisabled(twoFactorsAuthentication);
		}
	}

	private void addBruteForceButton() {
		Button bruteForcePrevention = new Button(I18N.message("bruteforceprevention"));
		bruteForcePrevention.setWidth100();
		bruteForcePrevention.setHeight(25);
		bruteForcePrevention.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new BruteForcePanel()));
		if (Feature.visible(Feature.BRUTEFORCE_ATTACK_PREVENTION) && Session.get().isDefaultTenant()
				&& Menu.enabled(Menu.BRUTEFORCE_ATTACK_PREVENTION)) {
			addMember(bruteForcePrevention);
			if (!Feature.enabled(Feature.BRUTEFORCE_ATTACK_PREVENTION) || Session.get().isDemo())
				setFeatureDisabled(bruteForcePrevention);
		}
	}

	private void addFirewallButton() {
		Button firewall = new Button(I18N.message("firewall"));
		firewall.setWidth100();
		firewall.setHeight(25);
		firewall.addClickHandler(event -> AdminScreen.get().setContent(new FirewallPanel()));
		if (Feature.enabled(Feature.FIREWALL) && Session.get().isDefaultTenant() && Menu.enabled(Menu.FIREWALL)) {
			addMember(firewall);
			if (!Feature.enabled(Feature.FIREWALL) || Session.get().isDemo())
				setFeatureDisabled(firewall);
		}
	}

	private void addAntivirusButton() {
		Button antivirus = new Button(I18N.message("antivirus"));
		antivirus.setWidth100();
		antivirus.setHeight(25);
		antivirus.addClickHandler(event -> AdminScreen.get().setContent(new AntivirusPanel()));
		if (Feature.visible(Feature.ANTIVIRUS)) {
			addMember(antivirus);
			if (!Feature.enabled(Feature.ANTIVIRUS) || Session.get().isDemo())
				setFeatureDisabled(antivirus);
		}
	}
}