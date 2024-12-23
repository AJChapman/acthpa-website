---
title: Radios
---
<div class="inline-img" data-fancybox="gallery" href="/images/radios-2.jpg">
![](/images/radios-2_web.jpg)
</div>

## UHF Radios

Use of UHF radios is strongly encouraged at all ACTHPA sites.
The ACTHPA radio channel is *SAFA1* (472.125 MHz, 192.8Hz CTCSS tone squelch, narrow band (12.5 kHz)).

## Explanation of SAFA/HGFA Channels

In Australia we have [80 CB channels](https://en.wikipedia.org/wiki/UHF_CB#UHF_CB_band_plan), in the frequency range of 476.425 MHz to 477.4125 MHz.
But SAFA has purchased rights to use another UHF channel outside of this range, at 472.125 MHz.
Within that single frequency SAFA have nominated four CTCSS frequencies that pilots can use, so as to make the most of this single channel by dividing it up into four distinct sub-channels.
These sub-channels are not quite as good as having four distinct channels, because transmissions on any one of the four will lock others out for the duration of the transmission, even if they are using a different sub-channel^[Assuming Busy Channel Lockout (see glossary) is enabled, otherwise they will unknowingly transmit over the top of each other, also bad.].
To make matters slightly more complicated, we also have the option of using the SAFA frequency *without* tone squelch.
We refer to this as *SAFA Open*, or *HGFA Open*.

The full [SAFA UHF & VHF Radio Frequencies document](http://members.hgfa.asn.au/isonic-downloaddoc.php?docid=0X0X1X1X47cDNnTHFRcmdyam93bG5oUVF0V3FSdz09) can be found on the SAFA website.
This supersedes the [old HGFA UHF Radio Channel document](https://www.safa.asn.au/resources/HGFA_UHF_Radio_Channel.pdf).

### Interactions between the HGFA channels

The *SAFA Open* channel ignores CTCSS, so it can hear transmissions from any of the other SAFA channels:

|         When someone transmits on: | SAFA Open | SAFA1  | SAFA2  | SAFA3  | SAFA4  |
| ---------------------------------- | --------- | ------ | ------ | ------ | ------ |
| **Can it be heard on:**            |           |        |        |        |        |
| **SAFA Open**                      | Yes       | Yes    | Yes    | Yes    | Yes    |
| **SAFA1**                          | No        | Yes    | No     | No     | No     |
| **SAFA2**                          | No        | No     | Yes    | No     | No     |
| **SAFA3**                          | No        | No     | No     | Yes    | No     |
| **SAFA4**                          | No        | No     | No     | No     | Yes    |

The result of this is that if you use *SAFA Open*, you will hear everyone, but only other *SAFA Open* users will hear you.
If you use any of the other SAFA channels then you will only hear other users of the same SAFA channel, and they will only hear you.

### Busy channel lockout on the SAFA channels

The SAFA channels require use of the busy channel lockout feature when using CTCSS.
This ensures that we don't unknowingly transmit over the top of each other when we're on different sub-channels of the SAFA frequency.
The use of busy channel lockout on *SAFA Open* is unnecessary, as you will be able to hear other pilots transmitting, and won't interrupt them.

## History of SAFA/HGFA Channels

Some time around October 2020 the [original HGFA channels](https://www.safa.asn.au/resources/HGFA_UHF_Radio_Channel.pdf) were replaced with the [new SAFA channels](http://members.hgfa.asn.au/isonic-downloaddoc.php?docid=0X0X1X1X47cDNnTHFRcmdyam93bG5oUVF0V3FSdz09).

The differences can be summarised for ACTHPA pilots as:

- Change channel bandwidth from 25 kHz to 12.5 kHz,
- Rename HGFA1, HGFA2, HGFA3 and HGFA4 to SAFA1, SAFA2, SAFA3 and SAFA4, respectively,
- Introduce 'SAFA 5 Open', which ACTHPA have previously referred to as HGFA Open, but which was not previously an official HGFA channel.

What this means for ACTHPA pilots is that we need to get our radios reprogrammed for the new frequencies, though the date for the changeover is unclear.
Fortunately the old channels and the new are roughly interoperable, though the quality and volume when using e.g. HGFA1 and SAFA1 may be impaired by the change in bandwidth.

## Glossary

- **Bandwidth**: The span of frequencies a radio transmission uses. When a radio is set to use a particular frequency (e.g. 472.125 MHz) it doesn't just transmit on this frequency; it transmits across a band of frequencies. For UHF radios this band is typically either 12.5 kHz (narrow band) or 25 kHz (wide band).
- **Busy Channel Lockout**: This radio feature stops you from transmitting if anyone else is transmitting on the same frequency. Without this feature enabled users would interfere with each other's messages if they transmitted at the same time, and would do this unknowingly when using different CTCSS frequencies.
- **Channel**: A channel is a particular radio configuration in which users can hear each other when they send and receive radio messages. In some cases this can just mean that they are using the same UHF frequency. In other cases, such as when CTCSS is used, it is when they share the same UHF frequency and the same CTCSS frequency. Note that when CTCSS is being used, multiple channels can share the same frequency.
- **CTCSS**: (from [Wikipedia](https://en.wikipedia.org/wiki/UHF_CB#CTCSS)) "Continuous tone coded squelch system (CTCSS) allows a group of radios set with the same tone to converse on a channel without hearing other radios using that channel. CTCSS can be used to silence a radio until another radio with the same tone transmits. This allows monitoring of a channel for transmissions from radios set with the same tone without hearing other conversations that use different or even no tone."
- **CTCSS Frequency**: A frequency in the range 67.0 Hz to 254.1 Hz, which defines a particular CTCSS sub-channel.
- **CTCSS Code Number**: Some radios (e.g. those made by Uniden) list their CTCSS frequencies by number, rather than by frequency. So, for example, the SAFA1, SAFA2, SAFA3, and SAFA4 CTCSS frequencies (192.8, 82.5, 110.9, 151.4 Hz) are CTCSS code numbers 31, 6, 15 and 24, respectively ([source](https://www.uniden.com.au/wp-content/uploads/OM_XTRAK_40.pdf)). Unfortunately these numbers are not standard across different radio manufacturers! For example, the ICOM numbers for the same frequencies are 39, 8, 17 and 26. So in general it is better to talk about CTCSS frequencies, rather than numbers.
- **CTCSS Transmit and CTCSS Receive**: Some radios allow you to set the CTCSS frequency you *transmit* with differently to the one you *receive* with. When programming one of these radios make sure you set both
- **DCS**: Digital-Coded Squelch. Also known as CDCSS, DTCS. A digital variation on CTCSS concept. Not used by ACTHPA.
- **Tone squelch**: See CTCSS.
- **UHF**: Ultra High Frequency, generally measured in megahertz (MHz). CB Radios are UHF radios.
