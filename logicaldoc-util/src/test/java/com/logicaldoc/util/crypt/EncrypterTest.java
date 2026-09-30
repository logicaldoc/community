package com.logicaldoc.util.crypt;

import static org.junit.Assert.assertNotEquals;

import org.junit.Test;

import com.logicaldoc.util.crypt.Encrypter;
import com.logicaldoc.util.crypt.Encrypter.EncryptionException;

import junit.framework.TestCase;

/**
 * Test case for {@link Encrypter}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since <product_release>
 */
public class EncrypterTest extends TestCase {

    private Encrypter testSubject;

    @Override
    protected void setUp() throws EncryptionException {
        testSubject = new Encrypter("9J850QhmZwz-Y*L)-.0g]Z&R,7I2d1f/");
    }

    @Test
    public void testEncrypt() throws EncryptionException {
        String clearString = "ciao mamma";
        String encryptedString = testSubject.encrypt(clearString);
        assertNotEquals(clearString, encryptedString);
        assertEquals(clearString, testSubject.decrypt("lsx218JrxzoVe7WclGb5Lg==:qKz+HmwAUazTFTHI:+m9Z/6sx/8Mr/foLXHLqS8gv1CYlAgbiZf4="));
    }
}