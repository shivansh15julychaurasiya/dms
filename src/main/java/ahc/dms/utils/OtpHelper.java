package ahc.dms.utils;

import ahc.dms.config.AppConstants;
import ahc.dms.exceptions.ApiException;
import ahc.dms.payload.OtpDto;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.net.URI;

@Component
public class OtpHelper {

    private final RestTemplate restTemplate = new RestTemplate();
    private final ObjectMapper objectMapper = new ObjectMapper();

    //username=&pass=&sender=HCALLD&sendto=8601837554&templateID=&message=OTP
    public void sendLoginOtp(String phone, String otp) {
        System.out.println("phone : "+phone);
        System.out.println("otp : "+otp);
        //URI
        UriComponentsBuilder builder = UriComponentsBuilder.fromUriString(AppConstants.LOGIN_OTP_URI)
                .queryParam("username", AppConstants.LOGIN_OTP_USERNAME)
                .queryParam("pass", AppConstants.LOGIN_OTP_PASSWORD)
                .queryParam("sender", AppConstants.LOGIN_OTP_SENDER)
                .queryParam("templateID", AppConstants.LOGIN_OTP_TEMPLATE_ID)
                .queryParam("sendto", phone)
                .queryParam("message", AppConstants.LOGIN_OTP_MSG_BEGIN + otp + AppConstants.LOGIN_OTP_MSG_END);

        URI uri = builder.build().encode().toUri();
        System.out.println("uri = "+uri.getRawQuery());

        String response = restTemplate.getForObject(uri, String.class);
        System.out.println("response = "+response);

        if (!response.contains("success")) {
            throw new ApiException(response);
        }

        /*
        OtpDto responseDto;
        try {
            responseDto = objectMapper.readValue(response, OtpDto.class);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }

        //ResponseEntity<OtpDto> response = restTemplate.getForEntity(uri, OtpDto.class);
        //OtpDto responseDto = response.getBody();
        return  responseDto;
        */

    }

}
