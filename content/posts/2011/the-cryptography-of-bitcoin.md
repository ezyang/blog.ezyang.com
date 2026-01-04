---
title: "The Cryptography of Bitcoin"
date: 2011-06-03 09:00:11
slug: the-cryptography-of-bitcoin
categories: [Cryptography]
comments:
    - id: 2568
      author: "The Cryptography of Bitcoin | Bitcoin News"
      date: "2011-06-03 14:16:48"
      content: "[...] by  ezyang   [link] [comment]  Friday, June 3rd, 2011 [...]"
    - id: 2573
      author: Gabriel
      date: "2011-06-03 16:47:51"
      content: |
        “Alice signs the transaction with her public key and publishes this signature for the Bitcoin network to see.”
        
        I guess you mean private key, don't you?
    - id: 2574
      author: Edward Z. Yang
      date: "2011-06-03 16:49:58"
      content: "Yes, fixed."
    - id: 2575
      author: yo
      date: "2011-06-03 16:57:39"
      content: "It is actually surprisingly difficult for a layperson to find out what precise English uses, without consulting the dictionary directly."
    - id: 2576
      author: Ivan
      date: "2011-06-03 17:10:26"
      content: "Great, so now we just need to have a quantum computer and we are all set!"
    - id: 2577
      author: midnightmagic
      date: "2011-06-04 02:46:46"
      content: "May I request clarification? Your preimage attack term suggests that there are specific targets that we mine to hit; but it is just a target @under@ which our resultant hash must arithmetically fall. If it does not, we increment a nonce and try again."
    - id: 2579
      author: Edward Z. Yang
      date: "2011-06-04 05:12:39"
      content: "Right, so I simply set my preimage attack against any of hash under the target hash, and get benefits. Actually, this means you don't need that strong of a preimage attack, but to my knowledge no one is studying that class of attacks."
    - id: 2584
      author: Joe Bloggs
      date: "2011-06-04 19:00:21"
      content: |
        &gt; "all of these primitives are hard-coded into the Bitcoin protocol"
        
        However:
        
        if blockNumber &gt; X
          use_new_hardcoded_primitives()
        else
          use_old_hardcoded_primitives()
        
        Here goes your "hard coded" argument.
    - id: 2586
      author: Edward Z. Yang
      date: "2011-06-04 19:10:38"
      content: "No, that constitutes a change in the protocol. In particular, any blocks after X will not work properly with clients speaking the old protocol."
    - id: 2589
      author: Joe Bloggs
      date: "2011-06-04 19:27:49"
      content: |
        &gt;No, that constitutes a change in the protocol. In particular, any blocks &gt;after X will not work properly with clients speaking the old protocol.
        
        It does constitute change of protocol. The clients which wish to stay in the network will have a choice to update or not therefore causing potential network split. 
        
        However, possibility of such upgrades and changes in protocol when needed and agreed upon my computational majority i.e. consensus does also constitutes flexibility and adaptability and ultimately survivability of bitcoin protocol and it's future reincarnations.
        
        BTW this would take care of quantum computer attacks too.
        
        Resistance is futile!
    - id: 2590
      author: Gavin Andresen
      date: "2011-06-04 19:29:11"
      content: |
        Nice summary of bitcoin crypto!
        
        If you're looking for weaknesses in bitcoin, I think the crypto is the wrong place to look.  I'm much more worried about denial of service attacks, Sybil attacks, viruses and trojans designed to steal users' wallets and scams and frauds.
        
        And what's wrong with changing the protocol if necessary?  Yes, old clients will stop working; assuming we give people at least a few months to upgrade the disruption should be small.  Has there ever been a modern crypto algorithm that was severely broken without at least a year of the theoretical weakness discovered / weakness expanded / special-case exploit demonstrated / general-case exploit created ?
        
        All that said:  if you have a brilliant, secure way of ensuring upward compatibility with future crypto algorithms then please submit a patch.
        
        And again, nice job describing what crypto bitcoin uses.
    - id: 2591
      author: Edward Z. Yang
      date: "2011-06-04 19:29:15"
      content: "Joe Bloggs: Non sequitur. Go reread the previous article. (You’ve just dressed up the centralized solution in different clothes.)"
    - id: 2592
      author: Edward Z. Yang
      date: "2011-06-04 19:36:54"
      content: |
        Gavin: In my previous essay, I describe why upgrading the crypto in Bitcoin is not like a normal upgrade of crypto for any other application. I'm not really sure what an upwards-compatible crypto-currency would look like—nor am I sure one is even theoretically possible.
        
        Bitcoin has a lot of challenges facing it, and crypto is one of the least of them, I agree. Consider this a crypto nerd post, learning and speculating about Bitcoin’s cryptography just for the heck of it :-)
    - id: 2594
      author: Momchil Georgiev
      date: "2011-06-06 09:14:53"
      content: |
        Edward, can you elaborate some more on the first preimage attack you mentioned?
        
        Valid bitcoin block hash is:
        
        sha256(
          sha256(
            - previous block hash;
            - the root of a merkle hash tree of all transactions included in this block. Each of the transactions must follow some rules in order to be 'valid';
            - timestamp, must be within certain limits for this particular block;
            - nonce, random 2^32 value; 
          )
        )
    - id: 2596
      author: Quora
      date: "2011-06-07 00:03:03"
      content: |
        <strong>Has Bitcoin been thoroughly reviewed by cryptographers and other open source code reviewers?...</strong>
        
        Yes! Although Bitcoin may be just taking off recently however the project was launched in 2009. There are 7 developers listed online[1], however the source code is hosted on github[2] with 28 contributors. Most notably the technology has been reviewed ...
    - id: 2942
      author: "An investment opportunity with the potential for tremendous social benefit | The Syntax of Things"
      date: "2011-08-31 03:20:00"
      content: "[...] A slightly longer description of how it works: http://blog.ezyang.com/2011/06/the-cryptography-of-bitcoin A USD exchange: https://www.bitcoin7.com/?ref=11837     &laquo; Proportionality in Just War [...]"
    - id: 3271
      author: "A Bitcoin Primer | CoinLab"
      date: "2011-12-28 19:32:59"
      content: "[...] Bitcoin protocol uses a combination of two of the strongest non-classified algorithms used by the NSA for encrypting Secret level documents. These are among the most secure published [...]"
    - id: 3568
      author: dln
      date: "2012-03-18 18:27:33"
      content: "I'm reading this trying to figure out if it is possible to use a bitcoin address as a public key for encrypting a message which will be unencryptable by the holder of that address. It looks like the answer is \"no\" but I'm still not sure about that."
    - id: 3826
      author: Bls
      date: "2012-06-16 03:21:06"
      content: "Thanks for your posts about Bitcoin protocols and crypto. There is some speculation that Bitcoin is merely a high-tech Ponzi scheme, that the elite programmers who conceived of it are in fact hoarding Bitcoins and merely waiting to cash them in to become millionaires. That there will be a Bitcoin Bill Gates are Mark Zuckerberg. I wonder. Anyway, right now I'm awed by the crypto involved and wonder if this is in fact the \"economic singularity\" some tout it to be. Predictions on Bitcoin saturation of world markets? Will, in your view, it collapse banks left and right? Thanks."
    - id: 5993
      author: "Bitcoin Mania &#8211; Key Learning | Continuous Learning &amp; Development"
      date: "2013-04-02 07:08:03"
      content: "[...] The Bitcoin Cryptography explained [...]"
    - id: 6307
      author: Bigbear11
      date: "2013-11-29 13:02:37"
      content: "Sorry, but SHA-256, RIPEMD-160, and Elliptic Curve DSA all have back doors!  That's why the NSA and government aren\"t worried about them!"
    - id: 6312
      author: Melchior
      date: "2013-12-01 15:58:48"
      content: "What gives Bitcoin such great value over other cryptocurrencies? Yes, there will only be 21mm Bitcoins, but if someone else creates an altcoin with better, or equal, encryption, doesn't this undermine the scarcity of bitcoin's value proposition? Is it just which coin build the best brand, or is its cryptography truly unique?"
    - id: 6345
      author: Adn
      date: "2014-01-08 03:29:31"
      content: |
        "Sorry, but SHA-256, RIPEMD-160, and Elliptic Curve DSA all have back doors! That’s why the NSA and government aren”t worried about them!"
        
        Nonsense. These are open source algorithms studied by hundreds of thousands of mathematicians everywhere in the world. There are no "backdoors" as these are not closed source system.
    - id: 6349
      author: begueradj
      date: "2014-01-10 15:17:21"
      content: |
        Hello,
        Can you give me a list of the main cryptographic algorithms used by Bitcoin ?
    - id: 6380
      author: "100th Post&#8230;discrete logs and cayley graphs | The Furloff"
      date: "2014-02-23 14:00:47"
      content: "[&#8230;] A discrete logarithm is the integer version of ordinary logarithms, and are principle objects in various practical applications of math and of are interest when it comes to the security of bitcoins. [&#8230;]"
    - id: 6417
      author: "Chapter 2: Smart Contracts | Great Wall of Numbers"
      date: "2014-03-04 11:36:47"
      content: "[&#8230;] set by secp256k1 (not the exploitable secp256r1).  See NSA Backdoors and Bitcoin by Chris Pacia, The Cryptography of Bitcoin by Edward Yang, An Overview of Elliptic Curve Cryptography by Julio López and Ricardo Dahab, ECDSA [&#8230;]"
    - id: 6559
      author: Joe Latreille
      date: "2014-03-18 18:42:57"
      content: "Very interesting, thanks"
    - id: 7881
      author: "How Does Bitcoin Cryptography Work | Want Bitcoin"
      date: "2014-09-11 19:52:36"
      content: "[&#8230;] The Cryptography of Bitcoin : Inside 206-105 – It is actually surprisingly difficult for a layperson to find out precisely what cryptography Bitcoin uses, without consulting the source of Bitcoin directly. &#8230; any blocks &gt;after X will not work properly with clients speaking the old protocol. It does constitute change of protocol. [&#8230;]"
    - id: 8089
      author: "What Was The First Bitcoin Transaction | Bitcoin Success"
      date: "2014-09-22 10:33:15"
      content: "[&#8230;] The Cryptography of Bitcoin : Inside 206-105 – Hashing in Bitcoin. This is the technically novel use of cryptography in Bitcoin, and it is used to answer the question, “With only traditional signatures, Alice &#8230;&#8230; [&#8230;]"
    - id: 8115
      author: "What Are Bitcoin Hashes Used For | Want Bitcoin"
      date: "2014-09-23 10:21:00"
      content: "[&#8230;] The Cryptography of Bitcoin : Inside 206-105 – &#8230; which I realized when commenter cruzer claimed that a break in the cryptographic hash would only reduce mining &#8230; I&#8217;m reading this trying to figure out if it is possible to use a bitcoin address as a public key for encrypting a message which will be unencryptable by the holder of &#8230; [&#8230;]"
    - id: 8590
      author: "What Is The Bitcoin Algorithm Used For | Want Bitcoin"
      date: "2014-10-04 23:14:36"
      content: "[&#8230;] The Cryptography of Bitcoin : Inside 206-105 – Can you give me a list of the main cryptographic algorithms used by Bitcoin ? &#8230; I&#8217;m reading this trying to figure out if it is possible to use a bitcoin address as a public key for encrypting a message which will be unencryptable by the holder of … […] [&#8230;]"
    - id: 18578
      author: "Nsa Created Bitcoin | Great P2P Currency"
      date: "2015-10-29 06:43:42"
      content: "[&#8230;] The Cryptography of Bitcoin : Inside 736-131 – Hashing in Bitcoin. This is the technically novel use of cryptography in Bitcoin, and it is used to answer the question, “With only traditional signatures, Alice &#8230; [&#8230;]"
    - id: 18939
      author: "Bitcoin Mining Algorithm Sha256 | Today&#039;s P2P Currency"
      date: "2015-11-11 22:39:59"
      content: "[&#8230;] The Cryptography of Bitcoin : Inside 736-131 – Hashing in Bitcoin. This is the technically novel use of cryptography in Bitcoin, and it is used to answer the question, “With only traditional signatures, Alice &#8230; [&#8230;]"
    - id: 22084
      author: "Bitcoin Quantum Mining | Guru Mutan - Share Digital Experience"
      date: "2017-08-17 19:42:37"
      content: "[&#8230;] The Cryptography of Bitcoin [&#8230;]"
    - id: 24319
      author: "Register Your Cash, Bitcoin, and Gift Certificates, Or... CIVIL FORFEITURE!!! - Debt to Success System - Debt to Success System"
      date: "2020-09-03 14:44:03"
      content: "[&#8230;] finally, crypto-wallets are&#8230; encrypted. If one does not volunteer one&#8217;s password, how could an agent determine the value of a [&#8230;]"
---

It is actually surprisingly difficult for a layperson to find out precisely what cryptography Bitcoin uses, without consulting the source of Bitcoin directly. For example, the [opcode OP_CHECKSIG](https://en.bitcoin.it/wiki/OP_CHECKSIG), ostensibly checks the signature of something... but there is no indication what kind of signature it checks! (What are opcodes in Bitcoin? Well it turns out that the protocol has a really neat scripting system built in for building transactions. [You can read more about it here.](https://en.bitcoin.it/wiki/Script)) So in fact, I managed to get some factual details wrong on my post [Bitcoin is not decentralized](http://blog.ezyang.com/2011/06/bitcoin-is-not-decentralized/), which I realized when commenter cruzer claimed that a break in the cryptographic hash would only reduce mining difficulty, and not allow fake transactions.

So I did my research and cracked open the Bitcoin client source code. The short story is that the thrust of my argument remains the same, but the details of a hypothetical attack against the cryptographic function are a bit more complicated—a simple chosen-prefix collision attack will not be sufficient. The long story? Bitcoin makes some interesting choices of the cryptography it chooses, and the rest of this post will explore those choices. Bitcoin makes use of two hashing functions, [SHA-256](http://en.wikipedia.org/wiki/SHA-2) and [RIPEMD-160](http://en.wikipedia.org/wiki/RIPEMD), but it also uses [Elliptic Curve DSA](http://en.wikipedia.org/wiki/Elliptic_Curve_DSA) on the curve secp256k1 to perform signatures. The C++ implementation uses a local copy of the Crypto++ library for mining, and OpenSSL for normal usage. At the end of this post, you should have a better understanding of how Bitcoin employs cryptography to simulate the properties of currency.

# Signatures in Bitcoin

In many ways, this is the traditional cryptography in Bitcoin. We ask the question, “How do we know that Alice was authorized to transfer 100 Bitcoins to Bob,” and anyone who has used public-key cryptography knows the answer is, “Alice signs the transaction with her private key and publishes this signature for the Bitcoin network to verify with her public key.” This signature is performed on the secp256k1 elliptic curve (`key.h`):

    CKey()
    {
        pkey = EC_KEY_new_by_curve_name(NID_secp256k1);
        if (pkey == NULL)
            throw key_error("CKey::CKey() : EC_KEY_new_by_curve_name failed");
        fSet = false;
    }

[Bitcoin community has discussed the choice of elliptic curve](http://forum.bitcoin.org/?topic=2699.0), and it appears this particular one was chosen for possible future [speed optimizations.](http://forum.bitcoin.org/index.php?topic=3238.0)

Like all public cryptography systems, however, Bitcoin does not sign the entire transaction message (that would be far too expensive); rather, it signs a cryptographic hash of the message (`script.cpp`):

    uint256 SignatureHash(CScript scriptCode, const CTransaction& txTo,
                          unsigned int nIn, int nHashType)
    {
        // ...
        // Serialize and hash
        CDataStream ss(SER_GETHASH);
        ss.reserve(10000);
        ss << txTmp << nHashType;
        return Hash(ss.begin(), ss.end());
    }

This hash is a *double* application of SHA-256:

    template<typename T1>
    inline uint256 Hash(const T1 pbegin, const T1 pend)
    {
        static unsigned char pblank[1];
        uint256 hash1;
        SHA256((pbegin == pend ? pblank : (unsigned char*)&pbegin[0]), (pend - pbegin) * sizeof(pbegin[0]), (unsigned char*)&hash1);
        uint256 hash2;
        SHA256((unsigned char*)&hash1, sizeof(hash1), (unsigned char*)&hash2);
        return hash2;
    }

Great, so how do we break this? There are several ways:

- We could break the underlying elliptic curve cryptography, by either solving the discrete logarithm problem (this is something quantum computers can do) or by breaking the particular elliptic curve that was chosen. Most research in this area goes towards finding vulnerabilities in specific elliptic curves, so the latter is more likely.
- We could break the underlying cryptographic hash function. In this case, we have a known signature from the user we would like to attack, and we generate another input transaction that hashes to the same value, so we can replay the previous signature. Such an attack would be dependent on the form of the serialized transaction that Bitcoin processes: it does a nontrivial amount of processing on a transaction, so some legwork by the attackers would be necessary; however, because transactions include a scripting system which permits complex transactions to be built, an attacker would have some leeway in constructing such an input. This would not work on single-use addresses, since no such signature exists for replay.

Breaking the signing algorithm requires a selective forgery attack or stronger, and means that arbitrary transactions may be forged and entered into the system. It would be a complete system break. For the signature replay attack, some protection could be gained by adding client-side checks that the same signature is never used for two different transactions.

# Hashing in Bitcoin

This is the technically novel use of cryptography in Bitcoin, and it is used to answer the question, “With only traditional signatures, Alice can resend bitcoins she doesn’t actually have as many times as she wants, effectively creating multiple branches of a transaction tree. How do we prevent this?” The answer Bitcoin provides is, “Transaction chains are certified by the solution of a computationally hard problem (mining), and once a transaction is confirmed by its inclusion in a block, clients prefer the transaction chain that has the highest computational cost associated with it, invalidating any other spending on other branches.” Even if you don’t believe in decentralized currency, you have to admit, this is pretty elegant.

In more detail, the computationally hard problem is essentially a watered-down version of the first-preimage attack on a hash function. Miners are given a set of solution hashes (the hash of all zeros to a target hash), and are required to find a message with particular structure (a chain of blocks plus a nonce) that hashes to one of these hashes.

In this case, it is easy to see that a first-preimage attack on a hash function (or perhaps a slightly weaker) attack means that this hashing problem can be solved much more quickly. This is a security break if an adversary knows this method but no one in the network does; he can easily then capture more than 50% of the network’s computing capacity and split the block chain (Remember: this is *exponential* leverage. I don’t care how many teraflops of power the Bitcoin network has—smart algorithms always win.) In a more serious break, he can rewrite history by reconstructing the entire block chain, performing enough “computational work” to convince other clients on the network that his history is the true one. This attack scenario is well known and is [described here.](https://en.bitcoin.it/wiki/Weaknesses#Attacker_has_a_lot_of_computing_power) Note that once the method is widely disseminated and adopted by other miners, the computational power imbalance straightens out again, and the difficulty of the hashing problem can be scaled accordingly.

# Addresses in Bitcoin

Similar to systems like PGP, Bitcoin users generate public and private keypairs for making signatures, but also publish a convenient “fingerprint”, actually a RIPEMD-160 hash for people to utilize as an identifier for a place you may send Bitcoin to (`util.h`):

    inline uint160 Hash160(const std::vector<unsigned char>& vch)
    {
        uint256 hash1;
        SHA256(&vch[0], vch.size(), (unsigned char*)&hash1);
        uint160 hash2;
        RIPEMD160((unsigned char*)&hash1, sizeof(hash1), (unsigned char*)&hash2);
        return hash2;
    }

*Unlike* systems like PGP, Bitcoin has no public key distribution mechanism: the RIPEMD-160 hash is canonical for a public key. As such, if a collision is discovered in this key space, someone could spend Bitcoins from someone else’s address. This attack scenario is [described here](https://en.bitcoin.it/wiki/Address). This attack is mitigated by the fact that Bitcoin users are encouraged to use many addresses for their wallet, and that other uses of such collision-power may be more profitable for the attacker (as described above.)

# Conclusion

As we can see, multiple different cryptographic primitives are used in ensemble in order to specify the Bitcoin protocol. Compromise of one primitive does not necessarily carry over into other parts of the system. However, all of these primitives are hard-coded into the Bitcoin protocol, and thus the arguments I presented [in my previous essay](http://blog.ezyang.com/2011/06/bitcoin-is-not-decentralized/) still hold.
