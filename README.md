# AWS EC2 PubKey Management

This package is a collection of CLI tools for managing AWS EC2 public
keys and SSH `known_hosts` files via the secure AWS API:

- `aws-ec2-pubkeys`: Use a JSON representation of EC2 instance IDs to query public keys.
- `aws-ec2-knownhosts`: Update the SSH `known_hosts` file with new host public keys.
- `aws-ec2-keysync`: Update the pubkey file on S3.

A properly configured `aws` command line tool is a requirement for
these tools to work.

## Create Hosts

Have the `aws_route53_record` resource in Terraform run a local
provisioner to output the newly created AWS instance IDs and DNS names:

    resource "aws_route53_record" "service" {
      count = "${var.service_count}"
      [...]
      provisioner "local-exec" {
        command = "touch aws-ec2-instances.json && echo '{\"init_key\": \""${var.key_name}"\", \"fqdn\": \"${aws_route53_record.public.fqdn}\", \"region\": \"${var.region}\", \"instance_id\": \"${element(aws_instance.service.*.id, count.index)}\", \"dns\": \"${element(aws_instance.service.*.private_dns, count.index)}\"}' >> aws-ec2-instances.json"
      }
    }

## Get Public Keys

Now that the AWS instance IDs of the newly created EC2 instances are
available in the `aws-ec2-instances.json` file, use the
`aws-ec2-pubkeys` Haskell CLI tool to query the AWS API for the public
keys of the included instances:

    aws-ec2-pubkeys aws-ec2-instances.json aws-ec2-pubkeys.json

Commit the `aws-ec2-pubkeys.json` file to a repository to keep the
public keys permanently. The public keys don't persist on the EC2
instance after reboot.

## Update Known Hosts

The Haskell CLI tool will clean out the SSH `known_hosts` file of old
public keys matching the hosts and then insert the newly queried host
keys:

    aws-ec2-knownhosts aws-ec2-pubkeys.json

## Update Shared Public Keys

All provisioned boxes' public keys need to be up-to-date and stored on s3. In
order to merge public key changes with the latest provision, the following command
can be run:

    aws-ec2-keysync <local_pubkey_file> <remote_pubkey_file> <s3_bucket_name>

    Ex. aws-ec2-keysync aws-ec2-pubkeys.json aws-ec2-pubkeys.json knownhosts

This will copy down the most up-to-date public key file from the specified environment,
merge in any changes from the local aws-ec2-pubkeys.json file, and copies the file
back into s3.

_Note: Currently, the pubkey file in each environment is named aws-ec2-pubkeys.json_
