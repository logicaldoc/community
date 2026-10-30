package com.logicaldoc.util.crypt;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ConvertEncryption {

    private static final Logger log = LoggerFactory.getLogger(ConvertEncryption.class);

    private String dbms = "";

    private String driver = "";

    private String url = "";

    private String username = "";

    private String password = System.getProperty("database.password");

    private String encryptionKey;

    private Encrypter encrypter;

    private Connection con;

    public static void main(String[] args) {
        ConvertEncryption convert = new ConvertEncryption();
        convert.setDriver(args[0]);
        convert.setUrl(args[1]);
        convert.setUsername(args[2]);
        convert.setPassword(args[3]);
        convert.setEncryptionKey(args[4]);

        convert.convert();
    }

    public ConvertEncryption() {
    }

    /**
     * Executes all the sql files defined in the constructor
     */
    public void convert() {
        try {
            encrypter = new Encrypter(encryptionKey);
            doConnection();
            convertEmailAccounts();
            convertImportFolders();
            convertLDAPs();
            convertKeystore();
            convertStamp();
        } catch (Exception e) {
            e.printStackTrace();
            log.error(e.getMessage(), e);
            try {
                if (con != null)
                    con.close();
            } catch (SQLException e1) {
                log.error(e1.getMessage(), e1);
            }
        }
    }

    private void convertEmailAccounts() throws SQLException {
        final String KEY = "0987jhatybscvc76452234ndaf8o90fdsnhbvcfds90823945ynbvf31lop.81234y9ik";

        List<String> oldSecrets = new ArrayList<>();
        List<String> oldPasswords = new ArrayList<>();
        try (PreparedStatement st = con.prepareStatement("select ld_clientsecret, ld_password from ld_emailaccount");
                ResultSet rs = st.executeQuery();) {
            while (rs.next()) {
                oldSecrets.add(rs.getString(1));
                oldPasswords.add(rs.getString(2));
            }
        }

        int secretCount = 0;
        for (String oldSecret : oldSecrets.stream().filter(s -> s != null && !s.isEmpty()).toList()) {
            try {
                String cleanOldSecret = new CryptUtil(KEY).decrypt(oldSecret);
                String newSecret = encrypter.encrypt(cleanOldSecret);
                if (newSecret != null && !newSecret.isEmpty())
                    try (PreparedStatement st = con.prepareStatement(
                            "update ld_emailaccount set ld_clientsecret = ? where ld_clientsecret = ?")) {
                        st.setString(1, newSecret);
                        st.setString(2, oldSecret);
                        st.execute();
                        secretCount++;
                    }
            } catch (Exception e) {
                log.error(e.getMessage(), e);
                e.printStackTrace();
            }
        }

        int passwordCount = 0;
        for (String oldPassword : oldPasswords.stream().filter(s -> s != null && !s.isEmpty()).toList()) {
            try {
                String cleanOldPassword = new CryptUtil(KEY).decrypt(oldPassword);
                String newPassword = encrypter.encrypt(cleanOldPassword);
                if (newPassword != null && !newPassword.isEmpty())
                    try (PreparedStatement st = con
                            .prepareStatement("update ld_emailaccount set ld_password = ? where ld_password = ?")) {
                        st.setString(1, newPassword);
                        st.setString(2, oldPassword);
                        st.execute();
                        passwordCount++;
                    }
            } catch (Exception e) {
                log.error(e.getMessage(), e);
                e.printStackTrace();
            }
        }

        System.out.println(
                "Encrypted %d secrets and %d passwords of email accounts".formatted(secretCount, passwordCount));
    }

    private void convertImportFolders() throws SQLException {
        final String KEY = "sjkl1fhrsjl.tg/,289o2n4!brfj1l23ti36453g2qwe42535";

        List<String> oldPasswords = new ArrayList<>();
        try (PreparedStatement st = con.prepareStatement("select ld_password from ld_importfolder");
                ResultSet rs = st.executeQuery();) {
            while (rs.next()) {
                oldPasswords.add(rs.getString(1));
            }
        }

        int passwordCount = 0;
        for (String oldPassword : oldPasswords.stream().filter(s -> s != null && !s.isEmpty()).toList()) {
            try {
                String cleanOldPassword = new CryptUtil(KEY).decrypt(oldPassword);
                String newPassword = encrypter.encrypt(cleanOldPassword);
                if (newPassword != null && !newPassword.isEmpty())
                    try (PreparedStatement st = con
                            .prepareStatement("update ld_importfolder set ld_password = ? where ld_password = ?")) {
                        st.setString(1, newPassword);
                        st.setString(2, oldPassword);
                        st.execute();
                        passwordCount++;
                    }
            } catch (Exception e) {
                log.error(e.getMessage(), e);
                e.printStackTrace();
            }
        }

        System.out.println("Encrypted %d passwords of import folders".formatted(passwordCount));
    }

    private void convertLDAPs() throws SQLException {
        final String KEY = "sjkl1fhrsjl.tg/,289o2n4!brfj1l23ti36453g2qwe42535";

        List<String> oldPasswords = new ArrayList<>();
        try (PreparedStatement st = con.prepareStatement("select ld_password from ld_ldapserver");
                ResultSet rs = st.executeQuery();) {
            while (rs.next()) {
                oldPasswords.add(rs.getString(1));
            }
        }

        int passwordCount = 0;
        for (String oldPassword : oldPasswords.stream().filter(s -> s != null && !s.isEmpty()).toList()) {
            try {
                String cleanOldPassword = new CryptUtil(KEY).decrypt(oldPassword);
                String newPassword = encrypter.encrypt(cleanOldPassword);
                if (newPassword != null && !newPassword.isEmpty())
                    try (PreparedStatement st = con
                            .prepareStatement("update ld_ldapserver set ld_password = ? where ld_password = ?")) {
                        st.setString(1, newPassword);
                        st.setString(2, oldPassword);
                        st.execute();
                        passwordCount++;
                    }
            } catch (Exception e) {
                log.error(e.getMessage(), e);
                e.printStackTrace();
            }
        }

        System.out.println("Encrypted %d passwords of LDAP servers".formatted(passwordCount));
    }

    private void convertKeystore() throws SQLException {
        final String KEY = "sjklfhrsjltg,289o2n4:123q4123r<abrfj1l23ti36453g2qwe42535";

        List<String> oldPasswords = new ArrayList<>();
        try (PreparedStatement st = con.prepareStatement("select ld_password from ld_keystore");
                ResultSet rs = st.executeQuery();) {
            while (rs.next()) {
                oldPasswords.add(rs.getString(1));
            }
        }

        int passwordCount = 0;
        for (String oldPassword : oldPasswords.stream().filter(s -> s != null && !s.isEmpty()).toList()) {
            try {
                String cleanOldPassword = new CryptUtil(KEY).decrypt(oldPassword);
                String newPassword = encrypter.encrypt(cleanOldPassword);
                if (newPassword != null && !newPassword.isEmpty())
                    try (PreparedStatement st = con
                            .prepareStatement("update ld_keystore set ld_password = ? where ld_password = ?")) {
                        st.setString(1, newPassword);
                        st.setString(2, oldPassword);
                        st.execute();
                        passwordCount++;
                    }
            } catch (Exception e) {
                log.error(e.getMessage(), e);
                e.printStackTrace();
            }
        }

        System.out.println("Encrypted %d passwords of Keystore".formatted(passwordCount));
    }
    
    private void convertStamp() throws SQLException {
        final String KEY = "vv4J33p67R7xA8PfdthB6GN969a98Y345hdS656AMCM8eZsMEV";

        List<String> oldImages = new ArrayList<>();
        try (PreparedStatement st = con.prepareStatement("select ld_image from ld_stamp");
                ResultSet rs = st.executeQuery();) {
            while (rs.next()) {
                oldImages.add(rs.getString(1));
            }
        }

        int imageCount = 0;
        for (String oldImage : oldImages.stream().filter(s -> s != null && !s.isEmpty()).toList()) {
            try {
                String cleanOldImage = new CryptUtil(KEY).decrypt(oldImage);
                String newImage = encrypter.encrypt(cleanOldImage);
                if (newImage != null && !newImage.isEmpty())
                    try (PreparedStatement st = con
                            .prepareStatement("update ld_stamp set ld_image = ? where ld_image = ?")) {
                        st.setString(1, newImage);
                        st.setString(2, oldImage);
                        st.execute();
                        imageCount++;
                    }
            } catch (Exception e) {
                log.error(e.getMessage(), e);
                e.printStackTrace();
            }
        }

        System.out.println("Encrypted %d images of Stamp".formatted(imageCount));
    }
    
    /**
     * Execute a SQL statements in the passed string
     * 
     * @param sql The SQL to execute
     */
    public void executeSql(String sql) {
        try {
            doConnection();
            try (PreparedStatement st = con.prepareStatement(sql)) {
                st.execute();
            }
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            log.error("Failed to execute %s".formatted(sql), e);
            try {
                if (con != null)
                    con.close();
            } catch (SQLException e1) {
                log.error(e1.getMessage(), e1);
            }
        }
    }

    protected void doConnection() {
        try {
            Class.forName(driver);
            con = DriverManager.getConnection(url, username, password);
            con.setAutoCommit(true);
        } catch (Exception ex) {
            ex.printStackTrace();
            log.error(ex.getMessage(), ex);
        }
    }

    protected void rollback() {
        try {
            con.rollback();
        } catch (Exception ex) {
            log.error("can't rollback", ex);
        }
    }

    /**
     * This method returns the state of the connection to the database.
     * 
     * @return checks true if the database is up and running and well connected
     */
    public boolean isConnected() {
        try {
            return !con.isClosed();
        } catch (Exception ex) {
            log.debug("db-connection is open: {}", ex.getMessage(), ex);
            return false;
        }
    }

    /**
     * This method tests a connection to the database
     * 
     * @return true if the database can be connected
     */
    public boolean testConnection() {

        boolean result = false;

        try {
            Class.forName(driver);
            con = DriverManager.getConnection(url, username, password);
            result = true;
            con.close();
        } catch (Exception ex) {
            log.error(ex.getMessage(), ex);
        }

        return result;
    }

    public void setDriver(String driver) {
        this.driver = driver;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getDbms() {
        return dbms;
    }

    public void setDbms(String dbms) {
        this.dbms = dbms;
    }

    public String getEncryptionKey() {
        return encryptionKey;
    }

    public void setEncryptionKey(String encryptionKey) {
        this.encryptionKey = encryptionKey;
    }
}