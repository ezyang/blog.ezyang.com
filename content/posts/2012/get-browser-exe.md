---
title: "GET /browser.exe"
date: 2012-10-12 14:26:17
slug: get-browser-exe
categories: [Computer Science]
comments:
    - id: 4355
      author: pwm
      date: "2012-10-12 15:34:28"
      content: "Apart from the speed of native execution, how is that different from making your website Flash or Java-based? And what makes it a good idea when using Flash or Java for a website isn't?"
    - id: 4357
      author: Anonymous
      date: "2012-10-12 16:48:58"
      content: "Why would anyone do this?  I don't understand what advantage your proposal has over just installing software traditionally -- in a VM, if you care about sandboxing."
    - id: 4358
      author: Anonymous
      date: "2012-10-12 16:54:23"
      content: |
        Having it be native code is a horrible idea for cross platform compatibility and future proofing.  The obvious approach then is some sort of VM, maybe a little like Java which was always intended to solve a problem very much like what you described.
        
        The other thing that I find hard to understand is precisely what is wrong with javascript (and all the languages that can transpile to it).  With canvas you can have complete control over the display.  I think it much more likely that native apps will be increasingly rewritten to run in html stacks than the other way around.
    - id: 4359
      author: Edward Z. Yang
      date: "2012-10-12 16:55:10"
      content: |
        pwm: Flash and Java require you to write specifically for those platforms. When native code is involved, you can use any platform you want; including Flash and Java (you just need to include the Flash Player or JVM along with the rest of your code!) Also, the Java sandbox is like swiss cheese, and no one takes it seriously; in some sense, it gives other sandbox methods a bad name.
        
        One of the primary objections to using Flash and Java is that they don't hold content in any standardized format; it's proprietary and opaque to third-party tools. But there's no reason they couldn't use a standardized format, the ecosystem just happens to discourage it. Now, discoverability and developer tools is certainly a problem in this scheme, since there's nothing compelling websites to support debuggability.
    - id: 4360
      author: Justin
      date: "2012-10-12 16:56:28"
      content: |
        What an excellent article! Thanks for writing about it.
        
        I suppose to get there you'd have to emulate existing browsers inside the container for some time.
        
        Hard to see it actually coming to pass, but this is some brilliant thinking IMO.
    - id: 4361
      author: Just some guy
      date: "2012-10-12 16:57:36"
      content: "So what we really want is a common platform to develop programs on. Since OS manufacturers could never agree on a common standard, more and more services moved to the web, and built a new ad hoc standard. This works surprisingly well, except that more and more features need to be integrated into the browser (e.g. OpenGL), and the process of rolling those out is inconsistent and slow. If we can just execute a binary on the host, that binary can just provide the support we need. But then we've come full-circle, so why do we need the browser at all? Could we just go back to making applications that provide the services we're interested in directly? For all the progress we've made moving everything to the web, sometimes it feels like we could've done more just improving the desktop in the first place :|"
    - id: 4362
      author: GingerPaul
      date: "2012-10-12 16:58:40"
      content: |
        Makes sense. Browsers are bloated with unused, outdated, buggy, broken and insecure pieces of code.
        
        The only real thing that needs to be secure is the Kernel, after that, let the site do what it wants to do. It'll be quicker than a normal browser because you don't have overhead of the code mentioned in the opening paragraph. Unless, you visit thousands of different websites each day that its.
        
        I like this idea!
    - id: 4363
      author: "you're strange"
      date: "2012-10-12 17:02:26"
      content: "It's called an executable, and has been around for 20 years. You download setup.exe, then install it, and WOW BOOYA SHAKA POW you've an executable that works reliably."
    - id: 4364
      author: anon
      date: "2012-10-12 17:07:17"
      content: |
        &gt; [all you need] is very little
        
        You're overlooking the kernel, or assuming most kernels have clean, friendly and safe interfaces. Though I can only speak about the FreeBSD kernel, my findings thus far have been that you really can't trust the kernel to be secure. Once an attacker has local access, regardless of what extra tripwires you've got, your entire stack is hosed.
        
        I think simple kernels are a common misconception because we don't realize how much cruft has accumulated over the years. POSIX is clean and wonderful, but no modern application can run with just a POSIX interface. FreeBSD (at least), has evolved some fairly nasty syscalls that are ubiquitous (sysctl, and anything that uses an iovec, particularly). I call those out, in particular, because they completely sidestep the relative safety of a type-checked system.
        
        Sure, we've got new technologies being developed (e.g., capcisum) that aim to fix these problems, but the route most taken is to add additional layers of abstraction rather than fix the underlying problems (e.g., SysV shared memory segment access permissions).
        
        And before someone suggests more heavy-handed virtualization, please note there have been HVM escalations (and other nasties) over the years. Running virtualized kernels isn't a panacea.
        
        While it is technically possible to develop sandboxed interfaces (where they're FreeBSD jails, Linux lxc or Windows execution contexts), it seems to me that we develop technical debt faster than we could ever hope to repair.
    - id: 4365
      author: "Maxime Chevalier-Boisvert"
      date: "2012-10-12 17:14:42"
      content: |
        I've thought about this before. It's similar to the app-store model that smartphones are currently using, but more decentralized (no central app store). Each website is a program, a piece of code at some URL that runs in some kind of client VM. It has definite advantages. Websites can use their own rendering engine, no need to be stuck with the whole HTML+JS model. If the VM is well-designed, almost native performance is possible. APIs can indeed be lower level, which I agree is safer, probably faster and also harder to screw up. Right now, browsers have a hard time all implementing the same DOM+JS+CSS semantics, partly because HTML5 is just (too) huge and getting huger.
        
        One issue is that, the web, right now, has hyperlinks. It's not entirely clear how well things would work out in an environment where every website has its own implementation. Would people still link to each other much? Would websites still have URLs that can change depending on content, or only one main URL you go to run the app? I guess it would depend on the individual websites, but there's the risk that in a web like that, websites would become isolated little islands.
        
        Another thing to consider is that realistically, it would take more effort than you make it sound. Right now, we're headed for a future where websites have the power of native apps. This means OpenGL access, APIs for input devices (mouse, keyboard, gamepads, clipboard), APIs for sound and things like webcam access, printing, other useful peripherals. The host VM could provide low level APIs for those things, but it couldn't not provide them, this would make this new model not competitive at all. The VM should also ideally provide some caching system for the software, and perhaps resource files (e.g.: images) the software might want to download, although I suppose if you provide some kind of local database or temporary filesystem, individual apps could use their own download and caching libraries.
        
        Anyways, I personally think it's a great idea... But, if it was put in the hands of corporations and standard bodies, it would likely become a very bloated very fast.
        The other issues is that corporations will never accept this idea until its already taken off. Believe it or not, I discussed this very idea with some people who work at Mozilla. They seemed utterly dismissive, with a "That's never gonna work! Nobody would ever switch to your parallel web!" kind of attitude. Their opinion is that we will be stuck with HTML+DOM+CSS+JS for the rest of eternity.
        
        I believe it has the potential to work, but someone needs to implement such a system and make it viable, prove that it works. Implement games with it, implement a web forum, a social network, or some kind of killer app people actually want, something that will make them download your client VM. Perhaps the smartest way to bootstrap such a system would be to provide a client for it written all in JavaScript, so that the rest of the web can have instant access to it without downloading the sandbox VM. At the same time, there could be an app for the sandbox VM that works as a regular HTML web browser.
    - id: 4366
      author: Sean Palmer
      date: "2012-10-12 17:38:40"
      content: "Hi, I thought you might be interested in this interview http://www.drdobbs.com/architecture-and-design/interview-with-alan-kay/240003442 especially page 2 where alan kay runs through some possibilities with this kind of system."
    - id: 4367
      author: Edward Z. Yang
      date: "2012-10-12 17:46:03"
      content: |
        Anonymous (1): So, you don't actually want to install it in a VM the traditional way, because if you do that, you *also* need to pull in an operating system, and that adds a huge amount of overhead that is just unnecessary. If you do it the lightweight way that Howell wants to do it, then you get basically equal performance (modulo swapping in the cache), and then it becomes a lot more practical. (Really; when's the last time you used a VM to sandbox?)
        
        Anonymous (2): Native code is remarkably, remarkably stable. Think about how carefully processors have to think about backwards compatibility when they add new features: if they decide to make a BC-breaking change, that's effectively introducing a new architecture. There aren't very many archs you have to support to get very good coverage: x86, x86_64, arm, and most of the VMs have excellent support for all of them, so you get it all for free!
        
        As for your second point, browsers are adding lots of features to give more "native-like" features, but all this does is increase the API surface area (and correspondingly the attack area). It also makes it harder to develop alternate browsers, and there are some fundamental things (e.g. JavaScript and its data and concurrency model) which simply will not be fixed by adding more function calls.
    - id: 4368
      author: Edward Z. Yang
      date: "2012-10-12 17:49:47"
      content: |
        GingerPaul: One thing you might empirically expect to occur is for most small, web-dev shops to use a set of common components. In that case, you can play tricks like share code segments between all of those websites, which would help reduce footprint. You would also see massively reduced footprints for software like Gmail, which could be dramatically slimmed down if you didn't have to work around limitations of your browser.
        
        you're strange: The primary difference is that for a long time we had no practical mechanisms for sandboxing executables. In the new world order, you would never click install.exe, because it would just run automatically.
    - id: 4369
      author: M$0NLY
      date: "2012-10-12 17:52:45"
      content: "It seems to work only on Microsoft Windows...  FAIL!!!"
    - id: 4370
      author: Edward Z. Yang
      date: "2012-10-12 17:54:09"
      content: "anon: Yes, exposing all of POSIX is a stupid idea. And you don't need it all anyway: why does a web application need ptrace or filesystem access? It doesn't make sense. The point is to keep it simple, because if we don't we have no chance of securing our interfaces, and one hypothesis of Howell's project is that at the low-level, we can keep things simple, because all of the bells and whistles are literally just userland libraries. That's the big idea!"
    - id: 4371
      author: Edward Z. Yang
      date: "2012-10-12 17:59:54"
      content: |
        Maxime: Good questions. With hyperlinks, one important thing that you would have to develop in the ecosystem is a standard for links that generalizes beyond just traditional webpages. But we have something like that already: it's called a URI. The extension necessary here is for it to be possible for custom schemes per domain name, but I don't think this is a particularly hard technical problem; it's primarily a social one, and since we live in a web where the hyperlink is so important, I think we have an edge (as opposed to people who were attempting to do this before we knew what the web looked like.)
        
        As for peripherals, all that you need to do is virtualize them properly (and there is already work here), and it "Just Works". The GPU is just another CPU, so you can use similar techniques to secure it as you would secure an ordinary CPU.
        
        M$0NLY: That's not true, they’re running Linux.
    - id: 4372
      author: Mark V.
      date: "2012-10-12 18:25:21"
      content: |
        The "promise" and the only reason for existence of the Internet — make information accessible to anyone — is basically ignored in favor of what exactly?
        
        With this, you effectively deny access to every system that cannot run your binary: alternative operating systems, systems running on other architectures, embedded, thin clients and whatnot.
        
        (Partially this problem could be solved with byte code, but not entirely.)
        
        Good thing this abomination will never succeed, I'd hate to go back to ActiveX-based web of 90s.
    - id: 4373
      author: Edward Z. Yang
      date: "2012-10-12 18:30:38"
      content: |
        Mark: I think the strong bias of a system like this towards non-openness is one reason why I am glad that virtualization didn't work when the web was initially taking off. You'll have to do a bit of work to make sure developer friendly APIs of the traditional kind are still available. But I think that because we know what the web looks like now, people are a lot more likely to think about how to make this possible, even if they do have the ability to run native code.
        
        (BTW, thin clients are very well supported by this: instead of running the stack on the client, run the stack on the server, and serve the pixels to the thin cilent.)
    - id: 4374
      author: Geoffrey Sneddon
      date: "2012-10-12 18:54:42"
      content: "And what happens when I try to view the \"non-website\" on a big-endian MIPS device? What are the odds of there being any binary for my device? Effectively you need something higher-level, but the moment you expose anything about the underlying hardware (say, endianness!) you open yourself up to untested configurations and footprint for finger-printing."
    - id: 4375
      author: Edward Z. Yang
      date: "2012-10-12 18:59:59"
      content: "As far as I can tell, Opera is the only major web browser which supports MIPS. So, that's the binary you should ship."
    - id: 4376
      author: Michael J. Ryan
      date: "2012-10-12 19:59:24"
      content: "Google already has a spec for this.. NaCl (Native Client), with a sandbox model...http://code.google.com/p/nativeclient/"
    - id: 4377
      author: Edward Z. Yang
      date: "2012-10-12 20:05:08"
      content: "Yeah, I asked Howell about NaCl, and he was like, \"Ha ha ha!\" It’s probably the most common comment, and there are two points: 1. NaCl has overhead of 30%, whereas we can get basically no overhead, and 2. NaCl doesn't propose to replace the entire stack, whereas Howell wants to do that."
    - id: 4380
      author: "Maxime Chevalier-Boisvert"
      date: "2012-10-12 22:21:07"
      content: |
        You need to define standardized, cross-platform APIs for the different kinds of common hardware devices. Counting on each OS to provide their own, or worse, the hardware vendors, would have terrible results: you would have loads of platform-specific web apps.
        
        The desirable minimum would probably include 2D/3D graphics. OpenGL could do both. Sound. Probably local filesystem access so you can work on your documents. Input devices, webcams, printing. Would be nice to support things like MIDI devices as well.
        
        I think that these APIs should all be kept simple and minimal. As few functions as possible, use of open standards. Printing could possibly be made postscript only. I don't think this system could work without such standardized APIs, however.
    - id: 4381
      author: lucian303
      date: "2012-10-12 23:28:27"
      content: |
        Hardly a radical departure from the Internet as you say: "What you gain from this radical departure from the original Internet is fine-grained control over all aspects of the application stack."
        
        Certainly a radical departure from the HTTP protocol indeed. 
        
        What I mean of course is that it's just a different type of architecture for a different type of Internet protocol. 
        
        Great article. Sparks my curiosity.
    - id: 4383
      author: anon
      date: "2012-10-13 01:05:48"
      content: |
        &gt; anon: Yes, exposing all of POSIX is a stupid idea. And you don’t need it all anyway: why does a web application need ptrace or filesystem access?
        
        I must admit, I only skimmed the paper. Presumably it involves running untrusted code natively, and "disallowing" specific kernel interfaces basically requires a code verifier (ala NaCl) or with explicit kernel-side support. It's not really a matter of userland libraries -- all they really boil down to is glue code between 'syscall' or 'int 0x80' invocations -- something that native code has no problem performing without library support.
        
        My argument was that current kernels are no where near capable of providing this support in a verifiable way. All the solutions I've seen have manifested as layers of abstraction and don't attempt to fix the underlying deficiencies.
        
        To conclude, unstable foundations have never stopped anyone in the past. I simply fear when this mountain of technical debt we've precariously perched upon comes crashing down.
    - id: 4384
      author: MegaMan.EXE
      date: "2012-10-13 02:04:08"
      content: "Untrusted-code-from-The-Cloud, SLOTTO IN!!"
    - id: 4385
      author: wereHamster
      date: "2012-10-13 03:34:10"
      content: |
        The way I see it, we use a certain programing language and it has access to certain APIs. Currently that would be JavaScript and the W3C APIs. What the paper advocates is assembler as the programing language, and "IP for communication beyond the process, and minimal low-level UI primitives to support the new display responsibilities identified above". This works for websites written in 1992, where the web browser is basically a glorified text viewer, with capabilities to quickly jump between websites.. But nowadays? The 'minimal low-level UI primitives' wouldn't be enough. The current JavaScript API provides so much more: geolocation, input, sound, file, camera, 3D, ... and much more is being worked on right now (https://wiki.mozilla.org/WebAPI).
        
        How long would it take until these 'minimal low-level UI primitives' would be extended with the new APIs? Web developers would very soon demand them. Then you'd run into the same problems as we have now, that vendors implement those API slightly differently. Incompatibility ensues. Would it be safer than what we have now? Maybe. But there would still be bugs. Just different bugs. Would that really be a better world than we have now?
    - id: 4386
      author: Stefan
      date: "2012-10-13 04:26:41"
      content: "Don't we already have this in our smart phones? App market, apps etc.. But I guess you're fishing for a more open standard."
    - id: 4389
      author: Singpolyma
      date: "2012-10-13 09:11:46"
      content: "This isn't an idea for a new Internew, but a new WWW, and even then not really but more just a new kind of browser for the WWW that is designed for SaaS apps instead of websites."
    - id: 4391
      author: Tianshuo
      date: "2012-10-13 21:10:11"
      content: This will be paradise for phishing attacks and hell for users.
    - id: 4398
      author: luka
      date: "2012-10-15 10:44:40"
      content: |
        What a horrible, horrible idea.
        Please make your websites in flash. It's not as bad as native code but it comes pretty close.
    - id: 4420
      author: Anonymous
      date: "2012-10-15 19:22:20"
      content: "Microsoft employee talking about security, very funny."
    - id: 4452
      author: Anonymous
      date: "2012-10-17 08:47:03"
      content: "Wow, time passes quick, we are already at April 1st?"
    - id: 4453
      author: Aria
      date: "2012-10-17 08:57:36"
      content: |
        The whole idea of the web is that it's an open platform: There is no machine code in it. Everyone writes in the same language, and everyone is involved in defining that language, and everyone understands it, and it runs on every media, and you can't hide your techniques ('cause your code is ultimately prettify-able), and that fosters innovation on the web.
        
        Imagine that you go to a website, and you see this cool, like, 3D effect, and there is no way for you to learn how the developer did it, because he has written on a proprietary platform.
        
        That's not open, it's just accessible.
        
        I think the problem you are trying to address here is the limitations of the standards, and the then the slow adoption of the standards. And I believe we should be looking for ways to fix that, without compromising the openness of the web.
    - id: 4560
      author: Jussi
      date: "2012-10-24 03:30:26"
      content: |
        Wonderful idea! Everyone knows that writing cross-platform native code is so much easier than writing cross-browser websites! Instead of having to take into account the horrors of IE, you'd just have to make different executables for different architectures (assuming the APIs are the same for each platform), such a life-changer!
        
        And you know, who cares if everyone stores the information on their websites in a proprietary format because who needs to search the web these days anyway!
        
        Sure, we'll lose all the accessibility of HTML, but meh, the disabled and old people don't deserve to use our applications anyway.
        
        And what a cross-platform way to tackle issues of extra functionality: plugins! If you want to do OpenGL, just write a proprietary plugin for all the platforms, simple as that! Or why bother, when you can just do DirectX!
    - id: 19388
      author: "HTML5 Weekly No.59 | ENUE"
      date: "2015-11-28 13:05:20"
      content: "[&#8230;] GET /browser.exe [&#8230;]"
---

[Jon Howell](http://research.microsoft.com/en-us/people/howell/) dreams of a new Internet. In this new Internet, cross-browser compatibility checking is a distant memory and new features can be unilaterally be added to browsers without having to convince the world to upgrade first. The idea which makes this Internet possible is so crazy, it just might work.

*What if a web request didn’t just download a web page, but the browser too?*

“That’s stupid,” you might say, “No way I’m running random binaries from the Internet!” But you’d be wrong: Howell knows how to do this, and furthermore, how to do so in a way that is *safer* than the JavaScript your browser regularly receives and executes. The idea is simple: the code you’re executing (be it native, bytecode or text) is not important, rather, it is the *system API* exposed to the code that determines the safety of the system.

Consider today’s browser, one of the most complicated pieces of software installed on your computer. It provides interfaces to “HTTP, MIME, HTML, DOM, CSS, JavaScript, JPG, PNG, Java, Flash, Silverlight, SVG, Canvas, and more”, all of which almost assuredly have bugs. The richness of the APIs are their own downfall, as far as security is concerned. Now consider what APIs a native client would need to expose, assuming that the website provided the browser and all of the libraries.

The answer is very little: all you need is a native execution environment, a minimal interface for persistent state, an interface for external network communication and an interface for drawing pixels on the screen (ala VNC). That’s it: everything else can be implemented as untrusted native code provided by the website. This is an interface that is small enough that we would have a hope of making sure that it is bug free.

What you gain from this radical departure from the original Internet is fine-grained control over all aspects of the application stack. Websites can write the equivalents of native apps (ala an App Store), but without the need to press the install button. Because you control the stack, you no longer need to work around browser bugs or missing features; just pick an engine that suits your needs. If you need push notifications, no need to hack it up with a poll loop, just implement it properly. Web standards continue to exist, but no longer represent a contract between website developers and users (who couldn’t care less about under the hood); they are simply a contract between developers and other developers of web crawlers, etc.

Jon Howell and his team have [implemented a prototype of this system](http://research.microsoft.com/apps/pubs/default.aspx?id=173709), and you can read more about the (many) technical difficulties faced with implementing a system like this. (Do I have to download the browser every time? How do I implement a Facebook Like button? What about browser history? Isn’t Google Native Client this already? Won’t this be slow?)

As a developer, I long for this new Internet. Never again would I have to write JavaScript or worry browser incompatibilities. I could manage my client software stack the same way I manage my server software stack, and use off-the-shelf components except in specific cases where custom software was necessary.) As a client, my feelings are more ambivalent. I can’t use Adblock or Greasemonkey anymore (that would involve injecting code into arbitrary executables), and it’s much harder for me to take websites and use them in ways their owners didn’t originally expect. (Would search engines exist in the same form in this new world order?) *Oh brave new world, that has such apps in't!*
